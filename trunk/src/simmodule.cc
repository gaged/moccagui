#include <math.h>
#include <stdio.h>
#include <string.h>
#include "canon.hh"
#include "rs274ngc.hh"


#define active_settings  interp_new.active_settings
#define active_g_codes   interp_new.active_g_codes
#define active_m_codes   interp_new.active_m_codes

CANON_PLANE              active_plane = CANON_PLANE_XY;
CANON_UNITS              length_unit_type = CANON_UNITS_MM;
double                   length_unit_factor = 1; /* 1 for MM 25.4 for inch */

static CANON_MOTION_MODE motion_mode = CANON_CONTINUOUS;
static CANON_DIRECTION   spindle_turning;

int                      active_slot = 1;
int                      line_number = 1;

static int               feed_mode = 0;
static double            feed_rate = 0.0;

static int               flood = 0;
static int               mist = 0;

double                   probe_position_a = 0; /*AA*/
double                   probe_position_b = 0; /*BB*/
double                   probe_position_c = 0; /*CC*/
double                   probe_position_x = 0;
double                   probe_position_y = 0;
double                   probe_position_z = 0;
double                   program_origin_a = 0; /*AA*/
double                   program_origin_b = 0; /*BB*/
double                   program_origin_c = 0; /*CC*/
double                   program_origin_x = 0;
double                   program_origin_y = 0;
double                   program_origin_z = 0;
double                   program_position_a = 0; /*AA*/
double                   program_position_b = 0; /*BB*/
double                   program_position_c = 0; /*CC*/
double                   program_position_x = 0;
double                   program_position_y = 0;
double                   program_position_z = 0;

static double            spindle_speed;
static double            traverse_rate;
static double            tool_xoffset, tool_zoffset, tool_woffset;

bool                     optionalprogram_stop = OFF;
bool                     block_delete = ON;
static double            motion_tolerance = 0.;
static double            naivecam_tolerance = 0.;

char                     _parameter_file_name[PARAMETER_FILE_NAME_LENGTH];

int                      tool_max = CANON_TOOL_MAX;
CANON_TOOL_TABLE         tools[CANON_TOOL_MAX];

static Interp interp_new;

extern "C" int interp_init() 
  { return interp_new.init(); }

extern "C" int interp_executecmd(const char *command, int line_no) 
  { return interp_new.execute(command,line_no); }

extern "C" int interp_execute()  
  { return interp_new.execute(); }

extern "C" int interp_exit()
  { return interp_new.exit(); }

extern "C" int interp_load_tool_table() 
  { return interp_new.load_tool_table(); }

extern "C" int interp_open(const char *filename) 
  { return interp_new.open(filename); }

extern "C" int interp_close() 
  { return interp_new.close(); }

extern "C" int interp_read(const char *mdi = 0) 
  { return interp_new.read(mdi); }

extern "C" int interp_reset()
  { return interp_new.reset(); }

extern "C" int interp_restore_parameters(const char *filename) 
  { return interp_new.restore_parameters(filename);}

extern "C" int interp_save_parameters(const char *filename, const double parameters[]) 
  { return interp_new.save_parameters(filename, parameters);}

extern "C" int interp_synch() 
  { return interp_new.synch(); }

extern "C" void interp_errortext(int error_code, char *error_text, int max_size)
  {return interp_new.error_text(error_code, error_text, max_size);}

// void setError(const char *fmt, ...);

extern "C" void interp_file_name(char *file_name, int max_size)
  { return interp_new.file_name(file_name, max_size); }

extern "C" int interp_line_length() 
  { return interp_new.line_length(); }

extern "C" void interp_line_text(char *line_text, int max_size)
  { return interp_new.line_text(line_text, max_size); }

extern "C" void setrotationxy(int line, double r);
extern "C" void setoriginoffsets(int line , double x, double y, double z, double a, double b,  double c);
extern "C" void settraverserate(int rate);
extern "C" void straigthtraverse(int line, double x, double y, double z, double a, double b, double c);
extern "C" void setUnits_inch();
extern "C" void setUnits_mm();
extern "C" void selectplane(CANON_PLANE in_plane);
extern "C" void splinefeed2(int line, double x1, double y1, double x2, double y2);
extern "C" void splinefeed3(int line, double x1, double y1, double x2, double y2, double x3, double y3);
extern "C" void arcfeed(int line, double first_end, double second_end,
 double first_axis, double second_axis, int rotation, double axis_end_point
 , double a, double b, double c, double u, double v, double w);
extern "C" void straightfeed(int line, double x, double y, double z, 
  double a, double b, double c, double u, double v, double w);
extern "C" void changetoolnumber(int slot);
extern "C" void changetool(int slot);
extern "C" void selecttool(int slot);
extern "C" void straightprobe(int line, double x, double y, double z, double a, double b, double c);
extern "C" void rigidtap(int, double x, double y, double z);   

extern "C" int settool(int slot, int id, double z, double dia, double xofs, double fang,
  double bang, int orient)
{
  if (slot < 0) {
    return 1;
  }
  else
    if (slot > tool_max) {
     return 1;
    }
  tools[slot].id = id;
  tools[slot].zoffset = z;
  tools[slot].diameter = dia;
  tools[slot].xoffset = xofs;
  tools[slot].frontangle = fang;
  tools[slot].backangle = bang;
  tools[slot].orientation = orient;
  return 0;
}

void SET_XY_ROTATION(double t) 
{ 
  setrotationxy(line_number++, t); 
}

void SET_ORIGIN_OFFSETS(
 double x, double y, double z
 , double a  /*AA*/
 , double b  /*BB*/
 , double c  /*CC*/
 , double u, double v, double w
)
{
  setoriginoffsets(line_number++,x,y,z,a,b,c);
  program_position_x = program_position_x + program_origin_x - x;
  program_position_y = program_position_y + program_origin_y - y;
  program_position_z = program_position_z + program_origin_z - z;
  program_position_a = program_position_a + program_origin_a - a;/*AA*/
  program_position_b = program_position_b + program_origin_b - b;/*BB*/
  program_position_c = program_position_c + program_origin_c - c;/*CC*/

  program_origin_x = x;
  program_origin_y = y;
  program_origin_z = z;
  program_origin_a = a;  /*AA*/
  program_origin_b = b;  /*BB*/
  program_origin_c = c;  /*CC*/
}

void USE_LENGTH_UNITS(CANON_UNITS in_unit)
{
  if (in_unit == CANON_UNITS_INCHES)
    {
      setUnits_inch();
      if (length_unit_type == CANON_UNITS_MM)
        {
          length_unit_type = CANON_UNITS_INCHES;
          length_unit_factor = 25.4;
          program_origin_x = (program_origin_x / 25.4);
          program_origin_y = (program_origin_y / 25.4);
          program_origin_z = (program_origin_z / 25.4);
          program_position_x = (program_position_x / 25.4);
          program_position_y = (program_position_y / 25.4);
          program_position_z = (program_position_z / 25.4);
        }
    }
  else if (in_unit == CANON_UNITS_MM)
    {
      setUnits_mm();
      if (length_unit_type == CANON_UNITS_INCHES)
        {
          length_unit_type = CANON_UNITS_MM;
          length_unit_factor = 1.0;
          program_origin_x = (program_origin_x * 25.4);
          program_origin_y = (program_origin_y * 25.4);
          program_origin_z = (program_origin_z * 25.4);
          program_position_x = (program_position_x * 25.4);
          program_position_y = (program_position_y * 25.4);
          program_position_z = (program_position_z * 25.4);
        }
    }
}

void SPLINE_FEED(
double x1, double y1, double x2, double y2, double x3, double y3)
{
  splinefeed3(line_number++,x1,y1,x2,y2,x3,y3);
  program_position_x = x3;
  program_position_y = y3;
}

void SPLINE_FEED(
double x1, double y1, double x2, double y2)
{
  splinefeed2(line_number++,x1,y1,x2,y2);
  program_position_x = x2;
  program_position_y = y2;
}

void ARC_FEED(int line_number, double first_end, double second_end,
 double first_axis, double second_axis, int rotation, double axis_end_point
 , double a, double b, double c, double u, double v, double w)
{
  arcfeed(line_number++,  first_end, second_end, first_axis, second_axis,
    rotation, axis_end_point, a, b, c, u, v, w);
  if (active_plane == CANON_PLANE_XY)
    {
      program_position_x = first_end;
      program_position_y = second_end;
      program_position_z = axis_end_point;
    }
  else if (active_plane == CANON_PLANE_YZ)
    {
      program_position_x = axis_end_point;
      program_position_y = first_end;
      program_position_z = second_end;
    }
  else /* if (active_plane == CANON_PLANE_XZ) */
    {
      program_position_x = second_end;
      program_position_y = axis_end_point;
      program_position_z = first_end;
    }
  program_position_a = a; /*AA*/
  program_position_b = b; /*BB*/
  program_position_c = c; /*CC*/
}

void STRAIGHT_FEED(int line_number, double x, double y, double z
 , double a, double b, double c, double u, double v, double w)
{
  straightfeed(line_number++, x, y, z, a, b, c, u, v, w);
  program_position_x = x;
  program_position_y = y;
  program_position_z = z;
  program_position_a = a;
  program_position_b = b;
  program_position_c = c;
}

/* This models backing the probe off 0.01 inch or 0.254 mm from the probe
point towards the previous location after the probing, if the probe
point is not the same as the previous point -- which it should not be. */

void STRAIGHT_PROBE(int line_number,
 double x, double y, double z
 , double a /*AA*/
 , double b /*BB*/
 , double c /*CC*/
 , double u, double v, double w, unsigned char probe_type)
{
  straightprobe(line_number++,x,y,z,a,b,c);
}


void RIGID_TAP(int line_number, double x, double y, double z)
{ rigidtap(line_number++, x,y,z); }

void DWELL(double seconds) {}


/* Free Space Motion */
void SET_TRAVERSE_RATE(double rate)
{
  settraverserate(rate);
  traverse_rate = rate;
}

void STRAIGHT_TRAVERSE( int line_number,
 double x, double y, double z
 , double a /*AA*/
 , double b /*BB*/
 , double c /*CC*/
 , double u, double v, double w
)
{
  straigthtraverse(line_number++,x,y,z,a,b,c);
  program_position_x = x;
  program_position_y = y;
  program_position_z = z;
  program_position_a = a; /*AA*/
  program_position_b = b; /*BB*/
  program_position_c = c; /*CC*/
}

void SET_FEED_MODE(int mode) { feed_mode = mode; }
void SET_FEED_RATE(double rate) { feed_rate = rate; }
//void SET_FEED_REFERENCE(CANON_FEED_REFERENCE reference) {}

extern void SET_MOTION_CONTROL_MODE(CANON_MOTION_MODE mode, double tolerance)
{
  motion_tolerance = 0;
  if (mode == CANON_EXACT_STOP) { motion_mode = CANON_EXACT_STOP; }
  else if (mode == CANON_EXACT_PATH) { motion_mode = CANON_EXACT_PATH; }
  else if (mode == CANON_CONTINUOUS) {
    motion_tolerance = tolerance;
    motion_mode = CANON_CONTINUOUS; }
}

extern void SET_NAIVECAM_TOLERANCE(double tolerance) { naivecam_tolerance = tolerance; }
void SELECT_PLANE(CANON_PLANE in_plane) { selectplane(in_plane); active_plane = in_plane; }
void SET_CUTTER_RADIUS_COMPENSATION(double radius) {}
void START_CUTTER_RADIUS_COMPENSATION(int side) {}
void STOP_CUTTER_RADIUS_COMPENSATION() { }
void START_SPEED_FEED_SYNCH() { }
void STOP_SPEED_FEED_SYNCH() { }
void SPINDLE_RETRACTtraverse() {}
void SET_SPINDLE_MODE(double arg) {}

void START_SPINDLE_CLOCKWISE()
  { spindle_turning = ((spindle_speed == 0) ? CANON_STOPPED : CANON_CLOCKWISE); }

void START_SPINDLE_COUNTERCLOCKWISE() 
  { spindle_turning = ((spindle_speed == 0) ? CANON_STOPPED : CANON_COUNTERCLOCKWISE); }
void SET_SPINDLE_SPEED(double rpm) { spindle_speed = rpm; }
void STOP_SPINDLE_TURNING() { spindle_turning = CANON_STOPPED; }
void SPINDLE_RETRACT() {}
void ORIENT_SPINDLE(double orientation, CANON_DIRECTION direction) {}
void USE_NOspindle_FORCE() {}

extern void SET_TOOL_TABLE_ENTRY(int toolno, double zoffset, double xoffset, double diameter,
  double frontangle, double backangle, int orientation) {}
extern void SET_TOOL_TABLE_ENTRY(int toolno, double zoffset, double diameter) {}
extern void SELECT_TOOL(int toolno) { selecttool(toolno); }

void USE_TOOL_LENGTH_OFFSET(double xoffset, double zoffset, double woffset) 
{
  tool_xoffset = xoffset;
  tool_zoffset = zoffset;
  tool_woffset = woffset;
}

void CHANGE_TOOL(int slot) { changetool(slot); active_slot = slot; }
void CHANGE_TOOL_NUMBER(int slot) { changetoolnumber(slot); active_slot = slot; }
void CLAMP_AXIS(CANON_AXIS axis) {}
void COMMENT(char *s) {}
// void DISABLE_ADAPTIVE_FEED() {}
void DISABLE_FEED_HOLD() {}
void DISABLE_FEED_OVERRIDE() {}
void DISABLE_SPEED_OVERRIDE() {}
// void ENABLE_ADAPTIVE_FEED() {}
void ENABLE_FEED_HOLD() {}
void ENABLE_FEED_OVERRIDE() {}
void ENABLE_SPEED_OVERRIDE() {}
void FLOOD_OFF() {}
void FLOOD_ON() {}
void INIT_CANON() {}
void MESSAGE(char *s) {}
void LOG(char *s) {}
void LOGOPEN(char *s) {}
void LOGCLOSE() {}
void MIST_OFF() {}
void MIST_ON() {}
void PALLET_SHUTTLE() {}
void TURN_PROBE_OFF() {}
void TURN_PROBE_ON() {}
void UNCLAMP_AXIS(CANON_AXIS axis) {}
void PROGRAM_STOP() {}
void SET_BLOCK_DELETE(bool state) {block_delete = state;}
bool GET_BLOCK_DELETE() {return block_delete;}
void SET_OPTIONAL_PROGRAM_STOP(bool state) {optionalprogram_stop = state;}
bool GET_OPTIONAL_PROGRAM_STOP() {return optionalprogram_stop;}
void OPTIONAL_PROGRAM_STOP() {}
void PROGRAM_END() {}
void SET_FEED_REFERENCE(int ref) {}
void DISABLE_ADAPTIVE_FEED() {}
void ENABLE_ADAPTIVE_FEED() {}
void START_SPEED_FEED_SYNCH(double sync, bool vel)  {}
int GET_EXTERNAL_AXIS_MASK() {return 0x3f; } // XYZABC machine
int GET_EXTERNAL_QUEUE_EMPTY() { return true; }
// extern int GET_EXTERNAL_QUEUE_EMPTY(){return 1;}

CANON_DIRECTION GET_EXTERNAL_SPINDLE() { return 0; }
// extern CANON_DIRECTION GET_EXTERNAL_SPINDLE(){return spindle_turning;}

CANON_PLANE GET_EXTERNAL_PLANE() { return 1; }
// SAI: CANON_PLANE GET_EXTERNAL_PLANE()  { return active_plane; }
double GET_EXTERNAL_FEED_RATE() { return 1; }
int GET_EXTERNAL_FEED_HOLD_ENABLE() {return 1;}
int GET_EXTERNAL_FEED_OVERRIDE_ENABLE() {return 1;}
int GET_EXTERNAL_ADAPTIVE_FEED_ENABLE() {return 0;}
double GET_EXTERNAL_TRAVERSE_RATE() { return 0; }
// double GET_EXTERNAL_TRAVERSE_RATE() {return traverse_rate;}

int GET_EXTERNAL_FLOOD() { return 0; }
int GET_EXTERNAL_MIST() { return 0; }
double GET_EXTERNAL_SPEED() { return 0; }

int GET_EXTERNAL_TOOL_MAX() { return CANON_TOOL_MAX; }
//extern int GET_EXTERNAL_TOOL_MAX(){return tool_max;}

int GET_EXTERNAL_TOOL_SLOT() { return 0; }
// extern int GET_EXTERNAL_TOOL_SLOT(){return active_slot;}

int GET_EXTERNAL_SELECTED_TOOL_SLOT() { return 0; }
//int    GET_EXTERNAL_SELECTED_TOOL_SLOT() {return active_slot; }
int GET_EXTERNAL_SPINDLE_OVERRIDE_ENABLE() {return 1;}
double GET_EXTERNAL_MOTION_CONTROL_TOLERANCE() { return motion_tolerance; }
double GET_EXTERNAL_LENGTH_UNITS() { return 0.03937007874016;}
double GET_EXTERNAL_ANGLE_UNITS() {return 1.0;}
CANON_UNITS GET_EXTERNAL_LENGTH_UNIT_TYPE() {return CANON_UNITS_MM;}
int GET_EXTERNAL_DIGITAL_INPUT(int index, int def) { return def; }
double GET_EXTERNAL_ANALOG_INPUT(int index, double def) { return def; }
int WAIT(int index, int input_type, int wait_type, double timeout) { return 0;}
extern CANON_MOTION_MODE GET_EXTERNAL_MOTION_CONTROL_MODE() { return motion_mode; }

void GET_EXTERNAL_PARAMETER_FILE_NAME(char * file_name, int max_size)
{
  if (0 == file_name)
    return;
  if (max_size < 0)
    return;
  if (strlen(_parameter_file_name) < (unsigned int)max_size)
    strcpy(file_name, _parameter_file_name);
  else
    file_name[0] = 0;
}

double GET_EXTERNAL_POSITION_A() {return program_position_a;}
double GET_EXTERNAL_POSITION_B() {return program_position_b;}
double GET_EXTERNAL_POSITION_C() {return program_position_c;}
double GET_EXTERNAL_POSITION_X() {return program_position_x;}
double GET_EXTERNAL_POSITION_Y() {return program_position_y;}
double GET_EXTERNAL_POSITION_Z() {return program_position_z;}
double GET_EXTERNAL_POSITION_U() {return 0.;}
double GET_EXTERNAL_POSITION_V() {return 0.;}
double GET_EXTERNAL_POSITION_W() {return 0.;}
double GET_EXTERNAL_PROBE_POSITION_U() {return 0.;}
double GET_EXTERNAL_PROBE_POSITION_V() {return 0.;}
double GET_EXTERNAL_PROBE_POSITION_W() {return 0.;}
double GET_EXTERNAL_PROBE_POSITION_A(){return probe_position_a;}
double GET_EXTERNAL_PROBE_POSITION_B(){return probe_position_b;}
double GET_EXTERNAL_PROBE_POSITION_C(){return probe_position_c;}
double GET_EXTERNAL_PROBE_POSITION_X(){return probe_position_x;}
double GET_EXTERNAL_PROBE_POSITION_Y(){return probe_position_y;}
double GET_EXTERNAL_PROBE_POSITION_Z(){return probe_position_z;}
extern double GET_EXTERNAL_PROBE_VALUE(){return 1.0;}
extern int GET_EXTERNAL_PROBE_TRIPPED_VALUE(){return 0;}
// double GET_EXTERNAL_SPEED(){return spindle_speed;}

extern CANON_TOOL_TABLE GET_EXTERNAL_TOOL_TABLE(int pocket){return tools[pocket];}
extern int GET_EXTERNAL_TLO_IS_ALONG_W(void) {return 0;}


USER_DEFINED_FUNCTION_TYPE USER_DEFINED_FUNCTION[USER_DEFINED_FUNCTION_NUM] = {0};

int USER_DEFINED_FUNCTION_ADD(USER_DEFINED_FUNCTION_TYPE func, int num)
{
  if (num < 0 || num >= USER_DEFINED_FUNCTION_NUM) {
    return -1;
  }
  USER_DEFINED_FUNCTION[num] = func;
  return 0;
}

void SET_MOTION_OUTPUT_BIT(int index){return;}
void CLEAR_MOTION_OUTPUT_BIT(int index){return;}
void SET_MOTION_OUTPUT_VALUE(int index, double value){return;}
void SET_AUX_OUTPUT_BIT(int index){return;}
void CLEAR_AUX_OUTPUT_BIT(int index){return;}
void SET_AUX_OUTPUT_VALUE(int index, double value){return;}
double GET_EXTERNAL_TOOL_LENGTH_ZOFFSET(){return tool_zoffset;}
double GET_EXTERNAL_TOOL_LENGTH_XOFFSET(){return tool_xoffset;}

void FINISH(void) {}
