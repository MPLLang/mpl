
#include "X11/Xlib.h"
#include "X11/Xutil.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int ml_creategc(Display * d, Window w) {

  XGCValues dummy;
  return (int)XCreateGC(d, w, 0, &dummy);

}

int ml_nextevent(Display * d, XEvent * * xe) {

  XEvent * x = (XEvent*) malloc(sizeof (XEvent));
  
  XNextEvent(d, x);

  *xe = x;

  return x->type;
}

int ml_maskevent(Display * d, long m, XEvent * * xe) {

  XEvent * x = (XEvent*) malloc(sizeof (XEvent));
  
  XMaskEvent(d, m, x);

  *xe = x;

  return x->type;
}

int ml_checkmaskevent(Display * d, long m, XEvent * * xe, int * typ) {

  XEvent * x = (XEvent*) malloc(sizeof (XEvent));
  
  if (XCheckMaskEvent(d, m, x)) {
    *typ = x->type;    
    *xe = x;
    return 1;
  } else {
    free (x);
    return 0;
  }

}

void ml_getmaprequest(XMapRequestEvent * xe, int * par, int * wnd) {
  
  *par = xe->parent;
  *wnd = xe->window;

}

void ml_getmapnotify(XMapEvent * xe, int * evt, int * wnd, int * orr) {
  
  *evt = xe->event;
  *wnd = xe->window;
  *orr = xe->override_redirect;

}

void ml_getnoexpose(XNoExposeEvent * xe, int * drw, int * maj, int * min) {
  
  *drw = xe->drawable;
  *maj = xe->major_code;
  *min = xe->minor_code;

}


void ml_getkeypress(XKeyEvent * xe, int * isdown, int * wnd, int * root, int * sub,
		    int * t, int * x, int * y, int * xr, int * yr, int * state, int * kc,
		    int * samescreen) {

  *isdown = xe->type == KeyPress;
  *wnd = xe->window;
  *root = xe->root;
  *sub = xe->subwindow;
  *t = 0; /* XXX */
  *x = xe->x;
  *y = xe->y;
  *xr = xe->x_root;
  *yr = xe->y_root;
  *state = xe->state;
  *kc = xe->keycode;
  *samescreen = xe->same_screen;

}

void ml_getbutton(XButtonEvent * xe, int * isdown, int * wnd, int * root, int * sub,
		  int * t, int * x, int * y, int * xr, int * yr, int * state, int * button,
		  int * samescreen) {
  
  *isdown = xe->type == ButtonPress;
  *wnd = xe->window;
  *root = xe->root;
  *sub = xe->subwindow;
  *t = 0; /* XXX */
  *x = xe->x;
  *y = xe->y;
  *xr = xe->x_root;
  *yr = xe->y_root;
  *state = xe->state;
  *button = xe->button;
  *samescreen = xe->same_screen;

}


void ml_getconfigurenotify(XConfigureEvent * xe, int * evt, int * wnd,
			   int * x, int * y, int * w, int * h,
			   int * b,
			   int * abo, int * orr) {
  *evt = xe->event;
  *wnd = xe->window;
  *x = xe->x;
  *y = xe->y;
  *w = xe->width;
  *h = xe->height;
  *b = xe->border_width;
  *abo = xe->above;
  *orr = xe->override_redirect;

}

void ml_getexpose(XExposeEvent * xe, int * wnd, int * x, int * y, int * w, int * h, int * ct) {

  *wnd = xe->window;
  *x = xe->x;
  *y = xe->y;
  *w = xe->width;
  *h = xe->height;
  *ct = xe->count;

}

void ml_eventstandard(XKeyEvent * xe, int * serial, int * sendevent, Display ** disp) {

  *serial = xe->serial;
  *sendevent = xe->send_event;
  *disp = xe->display;

}

void ml_getmotion(XMotionEvent * xe, int * wnd, int * root, int * sub,
		  int * t, int * x, int * y, int * xr, int * yr, int * state,
		  int * is_hint,
		  int * samescreen) {
  
  *wnd = xe->window;
  *root = xe->root;
  *sub = xe->subwindow;
  *t = 0; /* XXX */
  *x = xe->x;
  *y = xe->y;
  *xr = xe->x_root;
  *yr = xe->y_root;
  *state = xe->state;
  *is_hint = xe->is_hint;
  *samescreen = xe->same_screen;

}

void ml_drawtext_nofont(Display * display,
			Drawable d,
			GC gc,
			int x, int y,
			char * text,
			int len) {

  XTextItem xti;

  xti.chars = text;
  xti.nchars = len;
  xti.delta = 0;
  xti.font = None;

  XDrawText(display, d, gc, x, y, &xti, 1);

}


/* constants */

int MLX_Button1 = Button1;
int MLX_Button2 = Button2;
int MLX_Button3 = Button3;
int MLX_Button4 = Button4;
int MLX_Button5 = Button5;

int MLX_NoEventMask = NoEventMask;
int MLX_KeyPressMask = KeyPressMask;
int MLX_KeyReleaseMask = KeyReleaseMask;
int MLX_ButtonPressMask = ButtonPressMask;
int MLX_ButtonReleaseMask = ButtonReleaseMask;
int MLX_EnterWindowMask = EnterWindowMask;
int MLX_LeaveWindowMask = LeaveWindowMask;
int MLX_PointerMotionMask = PointerMotionMask;
int MLX_PointerMotionHintMask = PointerMotionHintMask;
int MLX_Button1MotionMask = Button1MotionMask;
int MLX_Button2MotionMask = Button2MotionMask;
int MLX_Button3MotionMask = Button3MotionMask;
int MLX_Button4MotionMask = Button4MotionMask;
int MLX_Button5MotionMask = Button5MotionMask;
int MLX_ButtonMotionMask = ButtonMotionMask;
int MLX_KeymapStateMask = KeymapStateMask;
int MLX_ExposureMask = ExposureMask;
int MLX_VisibilityChangeMask = VisibilityChangeMask;
int MLX_StructureNotifyMask = StructureNotifyMask;
int MLX_ResizeRedirectMask = ResizeRedirectMask;
int MLX_SubstructureNotifyMask = SubstructureNotifyMask;
int MLX_SubstructureRedirectMask = SubstructureRedirectMask;
int MLX_FocusChangeMask = FocusChangeMask;
int MLX_PropertyChangeMask = PropertyChangeMask;
int MLX_ColormapChangeMask = ColormapChangeMask;
int MLX_OwnerGrabButtonMask = OwnerGrabButtonMask;
