//    Based on a public domain work by Fred Proctor, Thomas Kramer, and
//    others at NIST
//    Copyright 2005-2009  Ray Henry, Paul Corner, John Kasunich,
//    Peter G. Vavaroutsos, Alex Joni, Chris Radek, Jeff Epler and others.
//
//    Copyright 2009-2010 Thomas Guenther
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//    Copyright 2004, 2005, 2006 Jeff Epler <jepler@unpythonic.net> and 
//    Chris Radek <chris@timeguy.com>
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "rs274ngc.hh"
#include "interp_return.hh"
#include "canon.hh"
#include "config.h"		// LINELEN
#include <string.h>		// strcmp
#include <sys/time.h>

#include <stdlib.h>
   
#ifdef VER_24
#include "rs274ngc_interp.hh"
#endif

Interp interp_new;

#define active_settings  interp_new.active_settings
#define active_g_codes   interp_new.active_g_codes
#define active_m_codes   interp_new.active_m_codes
#define interp_init	 interp_new.init
#define interp_open      interp_new.open
#define interp_close     interp_new.close
#define interp_read	 interp_new.read
#define interp_execute	 interp_new.execute
#define interp_synch     interp_new.synch
#define interp_exit      interp_new.exit
#define interp_reset     interp_new.reset

#define iserror(x) ((x) < 0 || (x) >= RS274NGC_MIN_ERROR)

char _parameter_file_name[LINELEN];

#ifdef VER_23
extern char *_rs274ngc_errors[];
#endif

#ifdef VER_24
EmcPose tool_offset;
#endif

#ifdef VER_23
double tool_xoffset, tool_zoffset, tool_woffset;
#endif

CANON_TOOL_TABLE canontool;

#include "toolparse.cc"

double settings[ACTIVE_SETTINGS];
int gcodes[ACTIVE_G_CODES];
int mcodes[ACTIVE_M_CODES];
int interp_error;
int last_sequence_number;
int plane;
bool metric;
double _pos_x, _pos_y, _pos_z, _pos_a, _pos_b, _pos_c, _pos_u, _pos_v, _pos_w;



int axis_mask;

extern "C" void nextline();
extern "C" void arcfeed(double first_end, double second_end, double first_axis, 
                        double second_axis, int rotation, double axis_end_point, 
                        double a_position, double b_position, double c_position, 
                        double u_position, double v_position, double w_position);
extern "C" void straightfeed(double x,double y,double z,double a,double b,
                             double c,double u,double v,double w);
extern "C" void straighttraverse(double x,double y,double z,double a,double b,double c,
                                 double u,double v,double w);
extern "C" void setoriginoffsets(double x,double y,double z,double a,double b,
                                 double c,double u,double v,double w);
extern "C" void setplane(int pl); 
extern "C" void settraverserate(double rate);
extern "C" void dwell(double time);
extern "C" void changetool(int tool);
extern "C" void setfeedrate(double rate);

#ifdef VER_23
extern "C" void tooloffset(double zoffset, double xoffset, double woffset);
#endif
#ifdef VER_24
extern "C" void tooloffset(double x, double y, double z, 
  double a, double b, double c, double u, double v, double w);
#endif

extern "C" int getblockdelete;

extern "C" void straightprobe(double x,double y,double z,double a,double b,double c,
                              double u,double v,double w);

extern "C" void rigidtap(double x,double y,double z);
extern "C" int gettool(int tool);
extern "C" void selecttool(int tool);

#ifdef VER_23
extern "C" int toolalongw();
#endif

extern "C" bool checkabort();

extern "C" double getLengthUnits();
extern "C" double getAngularUnits();

#ifdef VER_24
extern "C" void set_xy_rotation(double t);
#endif

extern "C" void userdefinedfunction(int num, double arg1, double arg2);

extern "C" void setmessage(char *msg);
extern "C" void setcomment(char *msg);


extern "C" int interpreter_init() { return interp_init(); }
extern "C" int interpreter_reset() { return interp_reset(); }

extern "C" void interpreter_codes()
{
  active_settings(settings);
  active_g_codes(gcodes);
  active_m_codes(mcodes);
}

extern "C" int interpreter_exec(char *command)
{
  return interp_execute(command);
}

extern "C" int interpreter_synch() { return interp_synch(); }

void maybe_new_line(int line_number) {
    if(interp_error) return;
    active_settings(settings);
    active_g_codes(gcodes);
    active_m_codes(mcodes);
    int sequence_number = line_number == -1? interp_new.sequence_number(): line_number;
    gcodes[0] = sequence_number;
    if(sequence_number == last_sequence_number) {
        return;
    }
    last_sequence_number = sequence_number;
    nextline;
}

void maybe_new_line(void) {
    maybe_new_line(-1);
}

#ifdef VER_24
void SET_XY_ROTATION(double t) {
    maybe_new_line();
    if(interp_error) return;
    set_xy_rotation(t);
};
#endif


#ifdef VER_24
void USE_TOOL_LENGTH_OFFSET(EmcPose offset) {
    tool_offset = offset;
    maybe_new_line();
    if(interp_error) return;
    if(metric) {
        offset.tran.x /= 25.4; offset.tran.y /= 25.4; offset.tran.z /= 25.4;
        offset.u /= 25.4; offset.v /= 25.4; offset.w /= 25.4; }
    tooloffset(offset.tran.x, offset.tran.y, offset.tran.z, 
        offset.a, offset.b, offset.c, offset.u, offset.v, offset.w);
}
#endif

#ifdef VER_23
void USE_TOOL_LENGTH_OFFSET(double xoffset, double zoffset, double woffset) {
    tool_zoffset = zoffset; tool_xoffset = xoffset; tool_woffset = woffset;
    maybe_new_line();
    if(interp_error) return;
    if(metric) { xoffset /= 25.4; zoffset /= 25.4; woffset /= 25.4; }
    tooloffset(zoffset, xoffset, woffset);
}
#endif

#ifdef VER_23
double GET_EXTERNAL_TOOL_LENGTH_XOFFSET() {
    return tool_xoffset;
}

double GET_EXTERNAL_TOOL_LENGTH_ZOFFSET() {
    return tool_zoffset;
}
#endif

#ifdef VER_24
double GET_EXTERNAL_TOOL_LENGTH_XOFFSET() {
    return tool_offset.tran.x;
}
double GET_EXTERNAL_TOOL_LENGTH_YOFFSET() {
    return tool_offset.tran.y;
}
double GET_EXTERNAL_TOOL_LENGTH_ZOFFSET() {
    return tool_offset.tran.z;
}
double GET_EXTERNAL_TOOL_LENGTH_AOFFSET() {
    return tool_offset.a;
}
double GET_EXTERNAL_TOOL_LENGTH_BOFFSET() {
    return tool_offset.b;
}
double GET_EXTERNAL_TOOL_LENGTH_COFFSET() {
    return tool_offset.c;
}
double GET_EXTERNAL_TOOL_LENGTH_UOFFSET() {
    return tool_offset.u;
}
double GET_EXTERNAL_TOOL_LENGTH_VOFFSET() {
    return tool_offset.v;
}
double GET_EXTERNAL_TOOL_LENGTH_WOFFSET() {
    return tool_offset.w;
}
#endif

void ARC_FEED(int line_number,
              double first_end, double second_end, double first_axis,
              double second_axis, int rotation, double axis_end_point,
              double a_position, double b_position, double c_position,
              double u_position, double v_position, double w_position) {
    // XXX: set _pos_*
    if(metric) {
        first_end /= 25.4;
        second_end /= 25.4;
        first_axis /= 25.4;
        second_axis /= 25.4;
        axis_end_point /= 25.4;
        u_position /= 25.4;
        v_position /= 25.4;
        w_position /= 25.4;
    }
    maybe_new_line(line_number);
    if(interp_error) return;
    arcfeed(first_end, second_end, first_axis, second_axis,rotation, 
            axis_end_point, a_position, b_position, c_position,
            u_position, v_position, w_position);
}

void STRAIGHT_FEED(int line_number,
                   double x, double y, double z,
                   double a, double b, double c,
                   double u, double v, double w) {
    _pos_x=x; _pos_y=y; _pos_z=z; 
    _pos_a=a; _pos_b=b; _pos_c=c;
    _pos_u=u; _pos_v=v; _pos_w=w;
    if(metric) { x /= 25.4; y /= 25.4; z /= 25.4; u /= 25.4; v /= 25.4; w /= 25.4; }
    maybe_new_line(line_number);
    if(interp_error) return;
    straightfeed(x, y, z, a, b, c, u, v, w);
}

void STRAIGHT_TRAVERSE(int line_number,
                       double x, double y, double z,
                       double a, double b, double c,
                       double u, double v, double w) {
    _pos_x=x; _pos_y=y; _pos_z=z; 
    _pos_a=a; _pos_b=b; _pos_c=c;
    _pos_u=u; _pos_v=v; _pos_w=w;
    if(metric) { x /= 25.4; y /= 25.4; z /= 25.4; u /= 25.4; v /= 25.4; w /= 25.4; }
    maybe_new_line(line_number);
    if(interp_error) return;
    straighttraverse(x, y, z, a, b, c, u, v, w);
}

#ifdef VER_24
static double TO_PROG_LEN(double p) {
    if(metric) return p*25.4;
    return p;
}

void NURBS_FEED(int line_number, std::vector<CONTROL_POINT> nurbs_control_points, unsigned int k) {
    double u = 0.0;
    unsigned int n = nurbs_control_points.size() - 1;
    double umax = n - k + 2;
    unsigned int div = nurbs_control_points.size()*15;
    std::vector<unsigned int> knot_vector = knot_vector_creator(n, k);	
    PLANE_POINT P1;
    while (u+umax/div < umax) {
        PLANE_POINT P1 = nurbs_point(u+umax/div,k,nurbs_control_points,knot_vector);
        // EBo -- replace 12345 with *whatever* gives us the line_number
        STRAIGHT_FEED(line_number, P1.X,P1.Y, _pos_z, _pos_a, _pos_b, _pos_c, _pos_u, _pos_v, _pos_w);
        u = u + umax/div;
    } 
    P1.X = nurbs_control_points[n].X;
    P1.Y = nurbs_control_points[n].Y;
    // EBo -- replace 12345 with *whatever* gives us the line_number
    STRAIGHT_FEED(line_number, P1.X,P1.Y, _pos_z, _pos_a, _pos_b, _pos_c, _pos_u, _pos_v, _pos_w);
    knot_vector.clear();
}

void SPLINE_FEED(int line_number, double x1, double y1, double x2, double y2) {
    double x0 = TO_PROG_LEN(_pos_x),
         y0 = TO_PROG_LEN(_pos_y);

    for(int i=1; i<=100; i++) {
      double t = i / 100.;
      double t2 = t*t;
      double t1 = 2*t*(1-t);
      double t0 = (1-t)*(1-t);
      double x = x0*t0 + x1*t1 + x2*t2;
      double y = y0*t0 + y1*t1 + y2*t2;
      // EBo -- replace 12345 with *whatever* gives us the line_number
      STRAIGHT_FEED(line_number, x,y, _pos_z, _pos_a, _pos_b, _pos_c, _pos_u, _pos_v, _pos_w);
    }
}

void SPLINE_FEED(int line_number, double x1, double y1, double x2, double y2, double x3, double y3) {
    double x0 = TO_PROG_LEN(_pos_x),
         y0 = TO_PROG_LEN(_pos_y);

    for(int i=1; i<=100; i++) {      double t = i / 100.;
      double t3 = t*t*t;
      double t2 = 3*t*t*(1-t);
      double t1 = 3*t*(1-t)*(1-t);
      double t0 = (1-t)*(1-t)*(1-t);
      double x = x0*t0 + x1*t1 + x2*t2 + x3*t3;
      double y = y0*t0 + y1*t1 + y2*t2 + y3*t3;
      STRAIGHT_FEED(line_number, x,y, _pos_z, _pos_a, _pos_b, _pos_c, _pos_u, _pos_v, _pos_w);
    }
}
#endif

void SET_ORIGIN_OFFSETS(double x, double y, double z,
                        double a, double b, double c,
                        double u, double v, double w) {
    if(metric) { x /= 25.4; y /= 25.4; z /= 25.4; u /= 25.4; v /= 25.4; w /= 25.4; }
    maybe_new_line();
    if(interp_error) return;
    setoriginoffsets(x, y, z, a, b, c, u, v, w);
}

void USE_LENGTH_UNITS(CANON_UNITS u) { metric = u == CANON_UNITS_MM; }
void SET_LENGTH_UNITS(CANON_UNITS u) { metric = u == CANON_UNITS_MM; }

void SELECT_PLANE(CANON_PLANE pl) {
    maybe_new_line();   
    if(interp_error) return;
    setplane(pl); 
}

void SET_TRAVERSE_RATE(double rate) {
    maybe_new_line();   
    if(interp_error) return;
    settraverserate(rate);
}

void SET_FEED_MODE(int mode) {
#if 0
    maybe_new_line();   
    if(interp_error) return;
    setfeedmode(mode);
#endif
}

void CHANGE_TOOL(int tool) {
    maybe_new_line();
    changetool(tool);
}

void CHANGE_TOOL_NUMBER(int tool) {
    maybe_new_line();
    if(interp_error) return;
}

void SET_FEED_RATE(double rate) {
    maybe_new_line();   
    if(interp_error) return;
    if(metric) rate /= 25.4;
    setfeedrate(rate);
}

void DWELL(double time) {
    maybe_new_line();   
    if(interp_error) return;
    dwell(time);
}

void LOG(char *s) {}
void LOGOPEN(char *f) {}
void LOGCLOSE() {}


#ifdef VER_23
void SET_TOOL_TABLE_ENTRY(int id, double zoffset, double xoffset, double diameter,
                          double frontangle, double backangle, int orientation) {
}
#endif

#ifdef VER_24
void SET_TOOL_TABLE_ENTRY(int pocket, int toolno, EmcPose offset, double diameter,
                          double frontangle, double backangle, int orientation) {
}
#endif

void SET_TOOL_TABLE_ENTRY(int id, double zoffset, double diameter) {
}


void SET_FEED_REFERENCE(double reference) { }
void SET_CUTTER_RADIUS_COMPENSATION(double radius) {}
void START_CUTTER_RADIUS_COMPENSATION(int direction) {}
void STOP_CUTTER_RADIUS_COMPENSATION(int direction) {}
void START_SPEED_FEED_SYNCH() {}
void START_SPEED_FEED_SYNCH(double sync, bool vel) {}
void STOP_SPEED_FEED_SYNCH() {}
void START_SPINDLE_COUNTERCLOCKWISE() {}
void START_SPINDLE_CLOCKWISE() {}
void SET_SPINDLE_MODE(double) {}
void STOP_SPINDLE_TURNING() {}
void SET_SPINDLE_SPEED(double rpm) {}
void ORIENT_SPINDLE(double d, int i) {}
void PROGRAM_STOP() {}
void PROGRAM_END() {}
void FINISH() {}
void PALLET_SHUTTLE() {}


#ifdef VER_23
void SELECT_TOOL(int tool) { selecttool(tool); }
#endif
#ifdef VER_24
void SELECT_POCKET(int tool) { selecttool(tool); }
#endif

void OPTIONAL_PROGRAM_STOP() {}

extern bool GET_BLOCK_DELETE(void) { 
    if(interp_error) return 0;
    return getblockdelete;
}

void DISABLE_FEED_OVERRIDE() {}
void DISABLE_FEED_HOLD() {}
void ENABLE_FEED_HOLD() {}
void DISABLE_SPEED_OVERRIDE() {}
void ENABLE_FEED_OVERRIDE() {}
void ENABLE_SPEED_OVERRIDE() {}
void MIST_OFF() {}
void FLOOD_OFF() {}
void MIST_ON() {}
void FLOOD_ON() {}
void CLEAR_AUX_OUTPUT_BIT(int bit) {}
void SET_AUX_OUTPUT_BIT(int bit) {}
void SET_AUX_OUTPUT_VALUE(int index, double value) {}
void CLEAR_MOTION_OUTPUT_BIT(int bit) {}
void SET_MOTION_OUTPUT_BIT(int bit) {}
void SET_MOTION_OUTPUT_VALUE(int index, double value) {}
void TURN_PROBE_ON() {}
void TURN_PROBE_OFF() {}

void STRAIGHT_PROBE(int line_number, 
                    double x, double y, double z, 
                    double a, double b, double c,
                    double u, double v, double w, unsigned char probe_type) {
    _pos_x=x; _pos_y=y; _pos_z=z; 
    _pos_a=a; _pos_b=b; _pos_c=c;
    _pos_u=u; _pos_v=v; _pos_w=w;
    if(metric) { x /= 25.4; y /= 25.4; z /= 25.4; u /= 25.4; v /= 25.4; w /= 25.4; }
    maybe_new_line(line_number);
    straightprobe(x, y, z, a, b, c, u, v, w);
}

void RIGID_TAP(int line_number,
               double x, double y, double z) {
    if(metric) { x /= 25.4; y /= 25.4; z /= 25.4; }
    maybe_new_line(line_number);
    rigidtap(x, y, z);
}
double GET_EXTERNAL_MOTION_CONTROL_TOLERANCE() { return 0.1; }
double GET_EXTERNAL_PROBE_POSITION_X() { return _pos_x; }
double GET_EXTERNAL_PROBE_POSITION_Y() { return _pos_y; }
double GET_EXTERNAL_PROBE_POSITION_Z() { return _pos_z; }
double GET_EXTERNAL_PROBE_POSITION_A() { return _pos_a; }
double GET_EXTERNAL_PROBE_POSITION_B() { return _pos_b; }
double GET_EXTERNAL_PROBE_POSITION_C() { return _pos_c; }
double GET_EXTERNAL_PROBE_POSITION_U() { return _pos_u; }
double GET_EXTERNAL_PROBE_POSITION_V() { return _pos_v; }
double GET_EXTERNAL_PROBE_POSITION_W() { return _pos_w; }
double GET_EXTERNAL_PROBE_VALUE() { return 0.0; }
int GET_EXTERNAL_PROBE_TRIPPED_VALUE() { return 0; }
double GET_EXTERNAL_POSITION_X() { return _pos_x; }
double GET_EXTERNAL_POSITION_Y() { return _pos_y; }
double GET_EXTERNAL_POSITION_Z() { return _pos_z; }
double GET_EXTERNAL_POSITION_A() { return _pos_a; }
double GET_EXTERNAL_POSITION_B() { return _pos_b; }
double GET_EXTERNAL_POSITION_C() { return _pos_c; }
double GET_EXTERNAL_POSITION_U() { return _pos_u; }
double GET_EXTERNAL_POSITION_V() { return _pos_v; }
double GET_EXTERNAL_POSITION_W() { return _pos_w; }
void INIT_CANON() {}

void GET_EXTERNAL_PARAMETER_FILE_NAME(char *name, int max_size) 
{
   if (0 == name)
	return;
    if (max_size < 0)
	return;
  if (strlen(_parameter_file_name) < (unsigned int)max_size)
    strcpy(name, _parameter_file_name);
  else
    name[0] = 0;
}

int GET_EXTERNAL_LENGTH_UNIT_TYPE() { return CANON_UNITS_INCHES; }

#ifdef VER_23
CANON_TOOL_TABLE GET_EXTERNAL_TOOL_TABLE(int tool) {
    CANON_TOOL_TABLE t = {0,0,0,0,0,0,0};
    if(interp_error) return t;
    if (gettool(tool) == 0) {
      return t;
    }
    else {
      return canontool;
    }
}
#endif

#ifdef VER_24
CANON_TOOL_TABLE GET_EXTERNAL_TOOL_TABLE(int pocket) {
    CANON_TOOL_TABLE t = {-1,{{0,0,0},0,0,0,0,0,0},0,0,0,0};
    if(interp_error) return t;
    if (gettool(pocket) == 0) {
      return t;
    }
    else {
       return canontool;
    }
}
#endif

#ifdef VER_23
int GET_EXTERNAL_TLO_IS_ALONG_W(void) { 
    int is_along_w = 0;
    if(interp_error) return 0;
    return toolalongw();
}
#endif

int GET_EXTERNAL_DIGITAL_INPUT(int index, int def) { return def; }
double GET_EXTERNAL_ANALOG_INPUT(int index, double def) { return def; }

int WAIT(int index, int input_type, int wait_type, double timeout) { return 0;}
int WAIT(int index, int input_type, int wait_type, int dummy) { return 0;}


void user_defined_function(int num, double arg1, double arg2) {
    if(interp_error) return;
    maybe_new_line();
    userdefinedfunction(num, arg1, arg2);
}

void SET_FEED_REFERENCE(int ref) {}
int GET_EXTERNAL_QUEUE_EMPTY() { return true; }
CANON_DIRECTION GET_EXTERNAL_SPINDLE() { return 0; }
int GET_EXTERNAL_TOOL_SLOT() { return 0; }
int GET_EXTERNAL_SELECTED_TOOL_SLOT() { return 0; }
double GET_EXTERNAL_FEED_RATE() { return 1; }
double GET_EXTERNAL_TRAVERSE_RATE() { return 0; }
int GET_EXTERNAL_FLOOD() { return 0; }
int GET_EXTERNAL_MIST() { return 0; }
CANON_PLANE GET_EXTERNAL_PLANE() { return 1; }
double GET_EXTERNAL_SPEED() { return 0; }

#ifdef VER_24
int GET_EXTERNAL_POCKETS_MAX() { return CANON_POCKETS_MAX; }
#endif

#ifdef VER_23
int GET_EXTERNAL_TOOL_MAX() { return CANON_TOOL_MAX; }
#endif


void DISABLE_ADAPTIVE_FEED() {} 
void ENABLE_ADAPTIVE_FEED() {} 

int GET_EXTERNAL_FEED_OVERRIDE_ENABLE() {return 1;}
int GET_EXTERNAL_SPINDLE_OVERRIDE_ENABLE() {return 1;}
int GET_EXTERNAL_ADAPTIVE_FEED_ENABLE() {return 0;}
int GET_EXTERNAL_FEED_HOLD_ENABLE() {return 1;}

int GET_EXTERNAL_AXIS_MASK() {
  return axis_mask;
}


double GET_EXTERNAL_LENGTH_UNITS() {
  return getLengthUnits();
}

double GET_EXTERNAL_ANGLE_UNITS() {
  return getAngularUnits();
}

bool check_abort() {
  return checkabort;
}

USER_DEFINED_FUNCTION_TYPE USER_DEFINED_FUNCTION[USER_DEFINED_FUNCTION_NUM];

CANON_MOTION_MODE motion_mode;
void SET_MOTION_CONTROL_MODE(CANON_MOTION_MODE mode, double tolerance) { motion_mode = mode; }
void SET_MOTION_CONTROL_MODE(double tolerance) { }
void SET_MOTION_CONTROL_MODE(CANON_MOTION_MODE mode) { motion_mode = mode; }
CANON_MOTION_MODE GET_EXTERNAL_MOTION_CONTROL_MODE() { return motion_mode; }

#ifdef VER_24
void SET_NAIVECAM_TOLERANCE(double tolerance) { }
#endif

void MESSAGE(char *comment) {
    maybe_new_line();
    if(interp_error) return;
    setmessage(comment);
}

void COMMENT(char *comment) {
    maybe_new_line();
    if(interp_error) return;
    setcomment(comment);
}

#define RESULT_OK (result == INTERP_OK || result == INTERP_EXECUTE_FINISH)

extern "C" int parsefile(char *filename, char *unitcode, char *initcode)
{
    int error_line_offset = 0;
    struct timeval t0,t1;
    int wait = 1;

    for(int i=0; i<USER_DEFINED_FUNCTION_NUM; i++) 
        USER_DEFINED_FUNCTION[i] = user_defined_function;

    gettimeofday(&t0, NULL);

    metric=false;
    interp_error = 0;
    last_sequence_number = -1;

    _pos_x = _pos_y = _pos_z = _pos_a = _pos_b = _pos_c = 0;
    _pos_u = _pos_v = _pos_w = 0;

    interp_init();
    interp_open(filename);

    maybe_new_line();

    int result = INTERP_OK;
    if(unitcode) {
        result = interp_read(unitcode);
        if(!RESULT_OK) goto out_error;
        result = interp_execute();
    }
    if(initcode && RESULT_OK) {
        result = interp_read(initcode);
        if(!RESULT_OK) goto out_error;
        result = interp_execute();
    }
    while(!interp_error && RESULT_OK) {
        error_line_offset = 1;
        result = interp_read();
        gettimeofday(&t1, NULL);
        if(t1.tv_sec > t0.tv_sec + wait) {
            if(check_abort()) return 1;
            t0 = t1;
        }
        if(!RESULT_OK) break;
        error_line_offset = 0;
        result = interp_execute();
    }
out_error:
   interp_close();
   interp_exit();
   maybe_new_line();
   return result;
}


int maxerror = -1;

#ifdef VER_23
static int find_maxerror(void) {
  int i=0;
    for(;;i++) {
        if(!_rs274ngc_errors[i] || !strcmp(_rs274ngc_errors[i], "The End"))
            return i;
    }
}
#endif

char savedError[LINELEN+1];

extern "C" int converterror(int err)
 {
    #ifdef VER_23
    if(err < 0 || err >= maxerror) {
    return 0;
    }
    #endif
    interp_new.error_text(err, savedError, LINELEN);
    return 1;
}

#ifdef VER_23
extern "C" void initgcode(void) {
    maxerror = find_maxerror();
}
#endif
