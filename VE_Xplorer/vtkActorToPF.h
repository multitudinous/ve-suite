//=========================================================================
//#
//#  vtkActorToPF.h                                   ##   ##
//#                                                   ##   ## 
//#  Authors: Paul Rajlich                            #######
//#  Email: prajlich@ncsa.uiuc.edu                    ##   ##
//#  Date created:                                    ##   ##
//#
//#  Description: function prototypes
//#
//=========================================================================
//# Copyright (C) 1998-2000 Board of Trustees of the University of Illinois
//=========================================================================

#ifndef VTK_ACTOR_TO_PF_H
#define VTK_ACTOR_TO_PF_H
#ifdef _PERFORMER
#include <Performer/pf/pfTraverser.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pfutil.h>
#include "vtkActor.h"
#include "vtkPolyDataMapper.h"

#ifdef VTK44
#define VTK4
#endif

#ifdef VTK4
#include "vtkCellArray.h"
#endif
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cmath>
#ifndef WIN32
#include <unistd.h>
#else
#include <windows.h>
#endif
#include <fcntl.h>

// vtkActorToPF - translates vtkActor to pfGeode. If geode is NULL, new one
//   will be created. Optional verbose parameter prints debugging and
//   performance information.
pfGeode* vtkActorToPF(vtkActor *actor, pfGeode *geode = NULL, int verbose=0);

// helper functions
void vtkActorToGeoSets(vtkActor *actor, pfGeoSet *gsets[], int verbose);
pfGeoSet *processPrimitive(vtkActor *a, vtkCellArray *prims, int pType, int v);
void updateTexture(vtkActor *actor, pfGeoSet *gset, pfGeoState *gstate, int v);

#endif
#endif
