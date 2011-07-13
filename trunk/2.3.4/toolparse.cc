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


char *ttcomments[CANON_TOOL_MAX+1];
CANON_TOOL_TABLE toolTable[CANON_TOOL_MAX+1];


extern "C" bool SetCanonTool(int tool)
{
  if (tool >= 0 && tool < CANON_TOOL_MAX)
  { 
    canontool = toolTable[tool];
    return true; 
  }
  return false;
}	
   
extern "C" void InitToolTable()
{
  for(int i=0;  i<=CANON_TOOL_MAX; i++)
   {
    ttcomments[i] = (char *)malloc(CANON_TOOL_ENTRY_LEN);
  }
}

extern "C" void DoneToolTable()
{ 
  for(int i=0; i<CANON_TOOL_MAX; i++) {
    free(ttcomments[i]);
  }
}

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
