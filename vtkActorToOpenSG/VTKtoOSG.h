/*
VTKtoOSG.h
*/
#ifndef VTK_TO_OSG_H
#define VTK_TO_OSG_H

#include <OpenSG/OSGGeometry.h>
class vtkActor;

// Activate the OpenSG namespace
OSG_USING_NAMESPACE

// Takes a VTK actor and converts it to an OpenSG Geometry pointer
// The option parameter specifies what type of geometry to be used:
// 0 for GL_TRIANGLES, 1 for GL_POLYGON, 2 for GL_QUADS, or anything
// else for GL_POINTS. Note that GL_TRIANGLES is the most popular option
// followed by GL_POLYGON, etc.
GeometryPtr convert( vtkActor *actor, int choice );

#endif
