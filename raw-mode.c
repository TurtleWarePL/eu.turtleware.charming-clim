
/* This small program is written based on a tutorial found under URL:
   https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html */

#include <signal.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios *enable_raw() {
  struct termios *orig_termios = malloc(sizeof(struct termios));
  struct termios raw;
  tcgetattr(STDIN_FILENO, orig_termios);
  raw = *orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
  return orig_termios;
}

void disable_raw(struct termios *orig_termios) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, orig_termios);
  free(orig_termios);
}

struct sigaction *enable_sigwinch(void(*catch_function)(int)) {
  struct sigaction *old_action = malloc(sizeof(struct sigaction));
  struct sigaction new_action;
  new_action.sa_handler = catch_function;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;
  sigaction(SIGWINCH, &new_action, old_action);
  return old_action;
}

void disable_sigwinch(struct sigaction *old_action) {
  sigaction(SIGWINCH, old_action, NULL);
  free(old_action);
}
