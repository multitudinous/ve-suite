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

#include <Performer/pf/pfTraverser.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pfutil.h>
#include "vtkActor.h"
#include "vtkPolyDataMapper.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>

// extra includes needed by VTK4.3 (sjk)
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkProperty.h"
#include "vtkImageData.h"
#include "vtkTexture.h"

// vtkActorToPF - translates vtkActor to pfGeode. If geode is NULL, new one
//   will be created. Optional verbose parameter prints debugging and
//   performance information.
pfGeode* vtkActorToPF(vtkActor *actor, pfGeode *geode = NULL, int verbose=0);

// helper functions
void vtkActorToGeoSets(vtkActor *actor, pfGeoSet *gsets[], int verbose=0);
pfGeoSet *processPrimitive(vtkActor *a, vtkCellArray *prims, int pType, int v);
void updateTexture(vtkActor *actor, pfGeoSet *gset, pfGeoState *gstate, int v);

#endif
