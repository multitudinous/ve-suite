//C++ header - fIVE|Analyse - Copyright (C) 2002-2003 Michael Gronager, UNI-C
//Distributed under the terms of the GNU Library General Public License (LGPL)
//as published by the Free Software Foundation.

#ifndef VTKACTORTOOSG_H
#define VTKACTORTOOSG_H

#include <osg/Geode>
#include <osg/Geometry>

#include <vtk/vtkActor.h>
#include <vtk/vtkPolyDataMapper.h>
#include <vtk/vtkCellArray.h>

// vtkActorToOSG - translates vtkActor to osg::Geode. If geode is NULL, new one
//   will be created. Optional verbose parameter prints debugging and
//   performance information.
osg::Geode* vtkActorToOSG(vtkActor *actor, osg::Geode *geode = NULL, int verbose=0);

osg::Geometry *processPrimitive(vtkActor *a, vtkCellArray *prims, int pType, int v);

#endif
