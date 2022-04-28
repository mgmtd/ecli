/* Simple proxy from raw terminal to erlang server.
 */

#include <sys/socket.h>
#include <sys/select.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <term.h>
// #include <curses.h>

#define MAXLINE     4096

#define min(a,b)    ((a) < (b) ? (a) : (b))
#define max(a,b)    ((a) > (b) ? (a) : (b))

struct termios orig_termios;

static void die(const char *s) {
  perror(s);
  exit(1);
}

/*
Raw terminal handling taken from article on creating a simple editor:
https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html
*/
static void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1)
    die("tcsetattr");
}

static void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);

  struct termios raw = orig_termios;
  raw.c_iflag &= ~(ICRNL | INLCR | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST | ICRNL | INLCR);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

/* Unix domain socket client taken from
   https://stackoverflow.com/questions/3324619/unix-domain-socket-using-datagram-communication-between-one-server-process-and */
static int openUnixDomSocket() {
  int sock_fd;
  char * server_filename = "/var/tmp/mgmtd.cli.socket";

  struct sockaddr_un server_addr;
  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sun_family = AF_UNIX;
  strncpy(server_addr.sun_path, server_filename, 104); // XXX: should be limited to about 104 characters, system dependent

  if ((sock_fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
    perror("create cli socket");
    return -1;
  }

  // connect client to server_filename
  if ((connect(sock_fd, (struct sockaddr *) &server_addr, sizeof(server_addr))) < 0) {
    perror("connect to server socket");
    return -1;
  }

  return sock_fd;

}


/* Append tgetstr result to buffer */
static char *getstr(char* buf, char *str, int len) {
  strncpy(buf, str, len);
  buf += len;
  strncpy(buf, ":", 1);
  buf++;
  if (tgetstr(str, &buf) !=0) {
    *(buf - 1) = ',';
  } else {
    *(buf) = ',';
    buf++;
  }
  return buf;
}

/* Append tgetflag result to buffer */
static char *getbool(char* buf, char *str, int len) {
  strncpy(buf, str, len);
  buf += len;
  strncpy(buf, ":", 1);
  buf++;
  if (tgetflag(str)) {
    *buf = '1';
    buf++;
    *buf = ',';
    buf++;
  } else {
    *buf = '0';
    buf++;
    *buf = ',';
    buf++;
  }
  return buf;
}


int main() {
  int sock_fd, maxfdp, val, stdineof;
  int istty;
  int rows, cols;
  ssize_t n, nwritten;
  fd_set rset, wset;
  char to[MAXLINE], fr[MAXLINE];
  char meta[MAXLINE];
  char *term;
  char *toiptr, *tooptr, *friptr, *froptr;
  int vsn = 1;

  sock_fd = openUnixDomSocket();
  if (sock_fd < 0) {
    exit(1);
  }

  istty = (isatty(STDIN_FILENO) && isatty(STDOUT_FILENO));

  if (istty) {
    enableRawMode();
  }

  if (istty) {
    struct winsize ws;
    if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) < 0) {
      perror("window size");
      exit(1);
    }

    rows = ws.ws_row;
    cols = ws.ws_col;
  }

  term = getenv("TERM");
  if (term == NULL) {
    term = "xterm";
  }

  // Get a selection of terminfo entries the server can use to drive
  // our raw terminal
  if (tgetent(fr, term) <=0) {
    perror("tgetent");
    exit(1);
  }

  snprintf(meta, MAXLINE-1, "cli:%d,%d,%d,%d,%s,", vsn, istty, rows, cols, term);

  int pos = strnlen(meta, MAXLINE);
  char *setpos = meta + pos;

  /* Add new blank line */
  setpos = getstr(setpos, "al", 2);

  /* Up command */
  setpos = getstr(setpos, "up", 2);

  /* Down command */
  setpos = getstr(setpos, "do", 2);

  /* Left command */
  setpos = getstr(setpos, "le", 2);

  /* Right command (non destructive) */
  setpos = getstr(setpos, "nd", 2);

  /* Clear screen and home cursor */
  setpos = getstr(setpos, "cl", 2);

  /* Clear to beginning of line */
  setpos = getstr(setpos, "cb", 2);

  /* Clear to end of line */
  setpos = getstr(setpos, "ce", 2);

  /* cursor to horiz pos */
  setpos = getstr(setpos, "ch", 2);

  /* Delete Character */
  setpos = getstr(setpos, "dc", 2);

  /* Delete Line */
  setpos = getstr(setpos, "dl", 2);

  /* Insert Character */
  setpos = getstr(setpos, "ic", 2);

  /* Insert Padding after inserted character */
  setpos = getstr(setpos, "ip", 2);

  /* Insert newline (like crlf) */
  setpos = getstr(setpos, "nw", 2);

  /* Down #1 lines */
  setpos = getstr(setpos, "DO", 2);

  /* Left #1 chars */
  setpos = getstr(setpos, "LE", 2);

  /* Right #1 chars */
  setpos = getstr(setpos, "RI", 2);

  /* UP #1 lines */
  setpos = getstr(setpos, "UP", 2);

  /* Clear to end of line */
  setpos = getstr(setpos, "ed", 2);

  /* Newline ignored after 80 cols */
  setpos = getbool(setpos, "xn", 2);

  *(setpos - 1) = 0;

  write(sock_fd, meta, strnlen(meta, MAXLINE));

  /* Straight from stevens */
  val = fcntl(sock_fd, F_GETFL, 0);
  fcntl(sock_fd, F_SETFL, val | O_NONBLOCK);

  val = fcntl(STDIN_FILENO, F_GETFL, 0);
  fcntl(STDIN_FILENO, F_SETFL, val | O_NONBLOCK);

  val = fcntl(STDOUT_FILENO, F_GETFL, 0);
  fcntl(STDOUT_FILENO, F_SETFL, val | O_NONBLOCK);

  toiptr = tooptr = to;       /* initialize buffer pointers */
  friptr = froptr = fr;
  stdineof = 0;

  maxfdp = max(max(STDIN_FILENO, STDOUT_FILENO), sock_fd) + 1;
  for ( ; ; ) {
    FD_ZERO(&rset);
    FD_ZERO(&wset);
    if (stdineof == 0 && toiptr < &to[MAXLINE])
      FD_SET(STDIN_FILENO, &rset);     /* read from stdin */
    if (friptr < &fr[MAXLINE])
      FD_SET(sock_fd, &rset);  /* read from socket */
    if (tooptr != toiptr)
      FD_SET(sock_fd, &wset);  /* data to write to socket */
    if (froptr != friptr)
      FD_SET(STDOUT_FILENO, &wset);   /* data to write to stdout */

    select(maxfdp, &rset, &wset, NULL, NULL);


    if (FD_ISSET(STDIN_FILENO, &rset)) {
      if ( (n = read(STDIN_FILENO, toiptr, &to[MAXLINE] - toiptr)) < 0) {
        if (errno != EWOULDBLOCK) {
          perror("read error on stdin");
          return -1;
        }

      } else if (n == 0) {
        fprintf(stderr, "EOF on stdin\r\n");
        stdineof = 1;   /* all done with stdin */
        if (tooptr == toiptr)
          shutdown(sock_fd, SHUT_WR);   /* send FIN */

      } else {
        // fprintf(stderr, "read %d bytes from stdin\r\n", n);
        toiptr += n;     /* # just read */
        FD_SET(sock_fd, &wset); /* try and write to socket below */
      }
    }

    if (FD_ISSET(sock_fd, &rset)) {
      if ( (n = read(sock_fd, friptr, &fr[MAXLINE] - friptr)) < 0) {
        if (errno != EWOULDBLOCK)
          perror("read error on socket");

      } else if (n == 0) {
        // fprintf(stderr, "EOF on socket\r\n");
        if (stdineof)
          return 0;     /* normal termination */
        else {
          // Server end closed. Either user ran an exit cmd or server died
          // FIXME - print something scary if the server died but not otherwise
          // perror("str_cli: server terminated prematurely");
          fprintf(stdout, "\r\n");
          exit(0);
        }

      } else {
        // fprintf(stderr, "read %d bytes from socket\r\n", n);
        friptr += n;     /* # just read */
        FD_SET(STDOUT_FILENO, &wset);     /* try and write below */
      }
    }

    if (FD_ISSET(STDOUT_FILENO, &wset) && ((n = friptr - froptr) > 0)) {
      if ( (nwritten = write(STDOUT_FILENO, froptr, n)) < 0) {
        if (errno != EWOULDBLOCK) {
          perror("write error to stdout");
          exit(1);
        }

      } else {
        // fprintf(stderr, "wrote %d bytes to stdout\r\n", nwritten);
        froptr += nwritten; /* # just written */
        if (froptr == friptr)
          froptr = friptr = fr;   /* back to beginning of buffer */
      }
    }

    if (FD_ISSET(sock_fd, &wset) && ((n = toiptr - tooptr) > 0)) {
      if ( (nwritten = write(sock_fd, tooptr, n)) < 0) {
        if (errno != EWOULDBLOCK)
          perror("write error to socket");

      } else {
        // fprintf(stderr, "wrote %d bytes to socket\r\n", nwritten);
        tooptr += nwritten; /* # just written */
        if (tooptr == toiptr) {
          toiptr = tooptr = to;   /* back to beginning of buffer */
          if (stdineof)
            shutdown(sock_fd, SHUT_WR);  /* send FIN */
        }
      }
    }
  }
}

/*
while (1) {
    char c = '\0';
    if (read(STDIN_FILENO, &c, 1) == -1 && errno != EAGAIN) die("read");
    if (iscntrl(c)) {
      printf("%d\r\n", c);
    } else {
      printf("%d ('%c')\r\n", c, c);
    }
    if (c == 'q') break;
  }

  return 0;
}
*/
