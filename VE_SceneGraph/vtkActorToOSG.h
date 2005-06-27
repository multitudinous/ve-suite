//C++ header - fIVE|Analyse - Copyright (C) 2002-2003 Michael Gronager, UNI-C
//Distributed under the terms of the GNU Library General Public License (LGPL)
//as published by the Free Software Foundation.

#ifndef VTKACTORTOOSG_H
#define VTKACTORTOOSG_H
#ifdef _OSG
#include <osg/Geode>
#include <osg/Geometry>

#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkCellArray.h>
#include "VE_Installer/VEConfig.h"
namespace VE_SceneGraph{
   // vtkActorToOSG - translates vtkActor to osg::Geode. If geode is NULL, new one
   //   will be created. Optional verbose parameter prints debugging and
   //   performance information.
   VE_SCENEGRAPH_EXPORTS osg::Geode* vtkActorToOSG(vtkActor* actor, osg::Geode *geode = NULL, int verbose=0);

   VE_SCENEGRAPH_EXPORTS osg::Geometry *processPrimitive(vtkActor *a, vtkCellArray *prims, int pType, int v);
}
#endif
#endif
