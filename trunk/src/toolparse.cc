// This is based on the original code taken from emc2 tool_parse.cc and iocontrol.cc
// some of the header- Files are not in the emc2/include tree so i decided to 
// use the original code to be compatible to different emc2 versions
// i.e. emctool.h is missing in /include

// Based on a public domain work by Fred Proctor, Thomas Kramer, and
// others at NIST
// Derived from a work by Fred Proctor & Will Shackleford
// Copyright 2005-2009  Ray Henry, Paul Corner, John Kasunich,
// Peter G. Vavaroutsos, Alex Joni, Chris Radek, Jeff Epler and others.
// Copyright 2009-2010 Thomas Guenther
//
// This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2 or 
//  later as published by the Free Software Foundation.

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "emcglb.h"

#ifdef VER_24
#include "emctool.h"
#endif

#ifdef VER_23
char *ttcomments[CANON_TOOL_MAX+1];
CANON_TOOL_TABLE toolTable[CANON_TOOL_MAX+1];
#endif

#ifdef VER_24
char *ttcomments[CANON_POCKETS_MAX+1];
CANON_TOOL_TABLE toolTable[CANON_POCKETS_MAX+1];
int fms[CANON_POCKETS_MAX + 1];
int random_toolchanger;
#endif

extern "C" bool SetCanonTool(int tool)
{
  #ifdef VER_24
  if (tool >= 0 && tool < CANON_POCKETS_MAX)
  #endif 
  #ifdef VER_23
  if (tool >= 0 && tool < CANON_TOOL_MAX)
  #endif 
  { 
    canontool = toolTable[tool];
    return true; 
  }
  return false;
}	
   
extern "C" void InitToolTable()
{
  #ifdef VER_23 
  for(int i=0;  i<=CANON_TOOL_MAX; i++)
  #endif
  #ifdef VER_24
  for(int i=0;  i<=CANON_POCKETS_MAX; i++)
  #endif
   {
    ttcomments[i] = (char *)malloc(CANON_TOOL_ENTRY_LEN);
  #ifdef VER_24
    fms[i] = 0;
  #endif
  }
}

extern "C" void DoneToolTable()
{
  #ifdef VER_23  
  for(int i=0; i<CANON_TOOL_MAX; i++) {
    free(ttcomments[i]);
  }
  #endif
  #ifdef VER_24
  for(int i=0; i<CANON_POCKETS_MAX; i++) {
    free(ttcomments[i]);
  }
  #endif
}

#ifdef VER_24
static bool scan_old_style(char *buffer,int &fakepocket) 
{
    int scanned, toolno, pocket, orientation;
    double zoffset, xoffset, diameter, frontangle, backangle;
    char comment[CANON_TOOL_ENTRY_LEN];

    if((scanned = sscanf(buffer, "%d %d %lf %lf %lf %lf %lf %d %[^\n]",
			 &toolno, &pocket, &zoffset, &xoffset, &diameter,
			 &frontangle, &backangle, &orientation, comment)) &&
       (scanned == 8 || scanned == 9)) {
	if(!random_toolchanger) {
	    fakepocket++;
	    if(fakepocket >= CANON_POCKETS_MAX) {
		printf("too many tools. skipping tool %d\n", toolno);
		return true;
	    }
	    if(fms) fms[fakepocket] = pocket;
	    pocket = fakepocket;
	}
	if (pocket < 0 || pocket >= CANON_POCKETS_MAX) {
	    printf("max pocket number is %d. skipping tool %d\n", CANON_POCKETS_MAX-1, toolno);
	    return true;
	} else {
	    /* lathe tool */
	    toolTable[pocket].toolno = toolno;
	    toolTable[pocket].offset.tran.z = zoffset;
	    toolTable[pocket].offset.tran.x = xoffset;
	    toolTable[pocket].diameter = diameter;

	    toolTable[pocket].frontangle = frontangle;
	    toolTable[pocket].backangle = backangle;
	    toolTable[pocket].orientation = orientation;
	    if(ttcomments && scanned == 9) strcpy(ttcomments[pocket], comment);
	    return true;
	}
    } else if ((scanned = sscanf(buffer, "%d %d %lf %lf %[^\n]",
				 &toolno, &pocket, &zoffset, &diameter, comment)) &&
	       (scanned == 4 || scanned == 5)) {
	if(!random_toolchanger) {
	    fakepocket++;
	    if(fakepocket >= CANON_POCKETS_MAX) {
		printf("too many tools. skipping tool %d\n", toolno);
		return true;
	    }
	    if(fms) fms[fakepocket] = pocket;
	    pocket = fakepocket;
	}
	if (pocket < 0 || pocket >= CANON_POCKETS_MAX) {
	    printf("max pocket number is %d. skipping tool %d\n", CANON_POCKETS_MAX-1, toolno);
	    return true;
	} else {
	    /* mill tool */
	    toolTable[pocket].toolno = toolno;
	    toolTable[pocket].offset.tran.z = zoffset;
	    toolTable[pocket].diameter = diameter;

	    // these aren't used on a mill
	    toolTable[pocket].frontangle = toolTable[pocket].backangle = 0.0;
	    toolTable[pocket].offset.tran.x = 0.0;
	    toolTable[pocket].orientation = 0;
	    if(ttcomments && scanned == 5) strcpy(ttcomments[pocket], comment);       
	    return true;
	}
    }
    return false;
}
#endif

#ifdef VER_24
extern "C" int loadToolTable(const char *filename)
{
    int fakepocket = 0;
    int t;
    FILE *fp;
    char buffer[CANON_TOOL_ENTRY_LEN];
    char orig_line[CANON_TOOL_ENTRY_LEN];
    int pocket = 0;

    if(!filename) return -1;

    // open tool table file
    if (NULL == (fp = fopen(filename, "r"))) {
	// can't open file
	return -1;
    }
    // clear out tool table
    for (t = random_toolchanger? 0: 1; t < CANON_POCKETS_MAX; t++) {
        toolTable[t].toolno = -1;
        ZERO_EMC_POSE(toolTable[t].offset);
        toolTable[t].diameter = 0.0;
        toolTable[t].frontangle = 0.0;
        toolTable[t].backangle = 0.0;
        toolTable[t].orientation = 0;
        if(fms) fms[t] = 0;
        if(ttcomments) ttcomments[t][0] = '\0';
    }

    /*
      Override 0's with codes from tool file
      File format is:

      <header>
      <pocket # 0..CANON_TOOL_MAX> <FMS id> <length> <diameter>
      ...

    */

    while (!feof(fp)) {
        const char *token;
        char *buff, *comment;
        int toolno, orientation, valid = 0;
        EmcPose offset;  // tlo
        double diameter, frontangle, backangle;

        // for nonrandom machines, just read the tools into pockets 1..n
        // no matter their tool numbers.  NB leave the spindle pocket 0
        // unchanged/empty.

        if (NULL == fgets(buffer, CANON_TOOL_ENTRY_LEN, fp)) {
            break;
        }
        strcpy(orig_line, buffer);

        if(scan_old_style(buffer, fakepocket)) continue;

        toolno = -1;
        diameter = frontangle = backangle = 0.0;
        orientation = 0;
        ZERO_EMC_POSE(offset);
        buff = strtok(buffer, ";");
        comment = strtok(NULL, "\n");

        token = strtok(buff, " ");
        while (token != NULL) {
            valid = 1;
            switch (toupper(token[0])) {
            case 'T':
                if (sscanf(&token[1], "%d", &toolno) != 1)
                    valid = 0;
                break;
            case 'P':
                if (sscanf(&token[1], "%d", &pocket) != 1) {
                    valid = 0;
                    break;
                }
                if (!random_toolchanger) {
                    fakepocket++;
                    if (fakepocket >= CANON_POCKETS_MAX) {
                        printf("too many tools. skipping tool %d\n", toolno);
                        valid = 0;
                        break;
                    }
                    if(fms) fms[fakepocket] = pocket;
                    pocket = fakepocket;
                }
                if (pocket < 0 || pocket >= CANON_POCKETS_MAX) {
                    printf("max pocket number is %d. skipping tool %d\n", CANON_POCKETS_MAX - 1, toolno);
                    valid = 0;
                    break;
                }
                break;
            case 'D':
                if (sscanf(&token[1], "%lf", &diameter) != 1)
                    valid = 0;
                break;
            case 'X':
                if (sscanf(&token[1], "%lf", &offset.tran.x) != 1)
                    valid = 0;
                break;
            case 'Y':
                if (sscanf(&token[1], "%lf", &offset.tran.y) != 1)
                    valid = 0;
                break;
            case 'Z':
                if (sscanf(&token[1], "%lf", &offset.tran.z) != 1)
                    valid = 0;
                break;
            case 'A':
                if (sscanf(&token[1], "%lf", &offset.a) != 1)
                    valid = 0;
                break;
            case 'B':
                if (sscanf(&token[1], "%lf", &offset.b) != 1)
                    valid = 0;
                break;
            case 'C':
                if (sscanf(&token[1], "%lf", &offset.c) != 1)
                    valid = 0;
                break;
            case 'U':
                if (sscanf(&token[1], "%lf", &offset.u) != 1)
                    valid = 0;
                break;
            case 'V':
                if (sscanf(&token[1], "%lf", &offset.v) != 1)
                    valid = 0;
                break;
            case 'W':
                if (sscanf(&token[1], "%lf", &offset.w) != 1)
                    valid = 0;
                break;
            case 'I':
                if (sscanf(&token[1], "%lf", &frontangle) != 1)
                    valid = 0;
                break;
            case 'J':
                if (sscanf(&token[1], "%lf", &backangle) != 1)
                    valid = 0;
                break;
            case 'Q':
                if (sscanf(&token[1], "%d", &orientation) != 1)
                    valid = 0;
                break;
            default:
                if (strncmp(token, "\n", 1) != 0)
                    valid = 0;
                break;
            }
            token = strtok(NULL, " ");
        }
        if (valid) {
            toolTable[pocket].toolno = toolno;
            toolTable[pocket].offset = offset;
            toolTable[pocket].diameter = diameter;
            toolTable[pocket].frontangle = frontangle;
            toolTable[pocket].backangle = backangle;
            toolTable[pocket].orientation = orientation;

            if (ttcomments && comment)
                strcpy(ttcomments[pocket], comment);
        } else {
            fprintf(stderr, "Unrecognized line skipped: %s", orig_line);
        }
        if (!random_toolchanger && toolTable[0].toolno == toolTable[pocket].toolno) {
            toolTable[0] = toolTable[pocket];
        }
    }

    // close the file
    fclose(fp);

    return 0;
}
#endif

#ifdef VER_24
extern "C" int saveToolTable(const char *filename)
{
    int pocket;
    FILE *fp;
    const char *name;
    int start_pocket;

    // check filename
    if (filename[0] == 0) {
	name = TOOL_TABLE_FILE;
    } else {
	// point to name provided
	name = filename;
    }

    // open tool table file
    if (NULL == (fp = fopen(name, "w"))) {
	// can't open file
	return -1;
    }

    if(random_toolchanger) {
        start_pocket = 0;
    } else {
        start_pocket = 1;
    }
    for (pocket = start_pocket; pocket < CANON_POCKETS_MAX; pocket++) {
        if (toolTable[pocket].toolno != -1) {
            fprintf(fp, "T%d P%d", toolTable[pocket].toolno, random_toolchanger? pocket: fms[pocket]);
            if (toolTable[pocket].diameter) fprintf(fp, " D%f", toolTable[pocket].diameter);
            if (toolTable[pocket].offset.tran.x) fprintf(fp, " X%+f", toolTable[pocket].offset.tran.x);
            if (toolTable[pocket].offset.tran.y) fprintf(fp, " Y%+f", toolTable[pocket].offset.tran.y);
            if (toolTable[pocket].offset.tran.z) fprintf(fp, " Z%+f", toolTable[pocket].offset.tran.z);
            if (toolTable[pocket].offset.a) fprintf(fp, " A%+f", toolTable[pocket].offset.a);
            if (toolTable[pocket].offset.b) fprintf(fp, " B%+f", toolTable[pocket].offset.b);
            if (toolTable[pocket].offset.c) fprintf(fp, " C%+f", toolTable[pocket].offset.c);
            if (toolTable[pocket].offset.u) fprintf(fp, " U%+f", toolTable[pocket].offset.u);
            if (toolTable[pocket].offset.v) fprintf(fp, " V%+f", toolTable[pocket].offset.v);
            if (toolTable[pocket].offset.w) fprintf(fp, " W%+f", toolTable[pocket].offset.w);
            if (toolTable[pocket].frontangle) fprintf(fp, " I%+f", toolTable[pocket].frontangle);
            if (toolTable[pocket].backangle) fprintf(fp, " J%+f", toolTable[pocket].backangle);
            if (toolTable[pocket].orientation) fprintf(fp, " Q%d", toolTable[pocket].orientation);
            fprintf(fp, " ;%s\n", ttcomments[pocket]);
        }
    }

    fclose(fp);
    return 0;
}
#endif

#ifdef VER_23
extern "C" int loadToolTable(char *filename)
{
    int t;
    FILE *fp;
    char buffer[CANON_TOOL_ENTRY_LEN];
    char comment[CANON_TOOL_ENTRY_LEN];
    const char *name;
    if (filename[0] == 0) {
	name = TOOL_TABLE_FILE;
    } else {
	name = filename;
    }
    if (NULL == (fp = fopen(name, "r"))) {
	return -1;
    }
    for (t = 0; t <= CANON_TOOL_MAX; t++) {
        toolTable[t].id = 0;
	toolTable[t].zoffset = 0.0;
	toolTable[t].diameter = 0.0;
        toolTable[t].xoffset = 0.0;
        toolTable[t].frontangle = 0.0;
        toolTable[t].backangle = 0.0;
        toolTable[t].orientation = 0;
        ttcomments[t][0] = '\0';
    }

    if (NULL == fgets(buffer, 256, fp)) {
	printf("Error: toolfile exists, but is empty\n");
	fclose(fp);
	return -1;
    }

    while (!feof(fp)) {
	int pocket;
	int id;
	double zoffset;
        double xoffset;
	double diameter;
        double frontangle, backangle;
        int orientation;
        int scanned;

	// just read pocket, ID, and length offset
	if (NULL == fgets(buffer, CANON_TOOL_ENTRY_LEN, fp)) {
	    break;
	}
        if((scanned = sscanf(buffer, "%d %d %lf %lf %lf %lf %lf %d %[^\n]",
                             &pocket, &id, &zoffset, &xoffset, &diameter,
                             &frontangle, &backangle, &orientation, comment)) &&
           (scanned == 8 || scanned == 9)) {
            if (pocket < 0 || pocket > CANON_TOOL_MAX) {
                printf("skipping tool: bad pocket number %d\n", pocket);
                continue;
            } else {
                /* lathe tool */
                toolTable[pocket].id = id;
                toolTable[pocket].zoffset = zoffset;
                toolTable[pocket].xoffset = xoffset;
                toolTable[pocket].diameter = diameter;

                toolTable[pocket].frontangle = frontangle;
                toolTable[pocket].backangle = backangle;
                toolTable[pocket].orientation = orientation;
                if(scanned == 9) strcpy(ttcomments[pocket], comment);
            }
        } else if ((scanned = sscanf(buffer, "%d %d %lf %lf %[^\n]",
                                     &pocket, &id, &zoffset, &diameter, comment)) &&
                   (scanned == 4 || scanned == 5)) {
            if (pocket < 0 || pocket > CANON_TOOL_MAX) {
                printf("skipping tool: bad pocket number %d\n", pocket);
                continue;
            } else {
                /* mill tool */
                toolTable[pocket].id = id;
                toolTable[pocket].zoffset = zoffset;
                toolTable[pocket].diameter = diameter;

                // these aren't used on a mill
                toolTable[pocket].frontangle = toolTable[pocket].backangle = 0.0;
                toolTable[pocket].xoffset = 0.0;
                toolTable[pocket].orientation = 0;
                //if(scanned == 5) strcpy(ttcomments[pocket], comment);
                if(scanned == 5) strcpy(ttcomments[pocket], comment);
                //printf("comment is %s \n", comment);
            }
        } else {
            /* invalid line. skip it silently */
            continue;
        }
    }

    // close the file
    fclose(fp);

    return 0;
}
#endif

#ifdef VER_23
extern "C" int saveToolTable(char *filename)
{
    int pocket;
    FILE *fp;
    const char *name;
    int lathe_style = 0;

    for(pocket=1; pocket <= CANON_TOOL_MAX; pocket++) {
        if(toolTable[pocket].orientation != 0) {
            lathe_style = 1;
            break;
        }
    }

    // check filename
    if (filename[0] == 0) {
	name = TOOL_TABLE_FILE;
    } else {
	// point to name provided
	name = filename;
    }

    // open tool table file
    if (NULL == (fp = fopen(name, "w"))) {
	// can't open file
	return -1;
    }

    if(lathe_style) {
        fprintf(fp, "%6s%4s%11s%11s%11s%12s%12s%7s  %s\n\n",
                "POCKET", "FMS", "ZOFFSET", "XOFFSET",
                "DIAMETER", "FRONTANGLE", "BACKANGLE", "ORIENT", 
                "COMMENT");
        for (pocket = 1; pocket <= CANON_TOOL_MAX; pocket++) {
            if (toolTable[pocket].id)
                fprintf(fp, "%6d%4d%+11f%+11f%11f%+12f%+12f%7d  %s\n",
                        pocket,
                        toolTable[pocket].id,
                        toolTable[pocket].zoffset, toolTable[pocket].xoffset, toolTable[pocket].diameter,
                        toolTable[pocket].frontangle, toolTable[pocket].backangle, 
                        toolTable[pocket].orientation, ttcomments[pocket]);
        }
    } else {
        fprintf(fp, "%7s%4s%11s%11s  %s\n\n",
                "POCKET", "FMS", "LENGTH", "DIAMETER", "COMMENT");
        for (pocket = 1; pocket <= CANON_TOOL_MAX; pocket++) {
            if (toolTable[pocket].id)
                fprintf(fp, "%7d%4d%+11f%11f  %s\n",
                        pocket,
                        toolTable[pocket].id,
                        toolTable[pocket].zoffset, toolTable[pocket].diameter,
                        ttcomments[pocket]);
        }
    }

    fclose(fp);
    return 0;
}
#endif
