/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdLaser.h,v $
 * Date modified: $Date: 2004/03/23 16:29:16 $
 * Version:       $Revision: 1.5 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_LASER
#define CFD_LASER

//#include "cfdObjects.h"

class vtkLineSource;
class vtkTubeFilter;
class vtkPolyDataNormals;
class vtkPolyDataMapper;
class vtkActor;
class pfDCS;
class pfGeode;

//! Create a laser beam.
/*!
  A class to build a virtual laser beaming from wand 
  using vtk functions and render using Performer.  
  VTK objects(vtkActor) are translated into Performer objects(pfGeode).
*/
class cfdLaser //: public cfdObjects
{
 public:
  //! Constructor
  /*!
    Construct vtk objects, pfGeode, and pfDCS.
  */
  cfdLaser( );
  //! Destructor
  /*!
    Destruct vtk objects, pfGeode, and pfDCS.
  */
  ~cfdLaser( );
  //!
  /*!
    Check whether the laser beam is hitting the menu.
    If non-zero, then it is true (hit).
    If return is 0, then it it false (miss).
    b is bounding box of the data set i.e. xmin, xmax, ymin, ymax, zmin, zmax.
    origin is the wand position.
    vec is the wand direction or vector.
  */
  int HitMenu( double b[6], double origin[3], double vec[3] );
  //!
  /*!
    Get the (x, y, z) point where the laser hit on the given data set.
  */
  double * GetHitXYZ( );
  //!
  /*!
    Get the dynamic coordinate system with pre-loaded vtkObjects.
  */
  pfDCS * GetpfDCS( );
  //!
  /*!
    Update the laser position and direction of ray.
  */
  void Update( double origin[3], double vec[3] );

 private:
  //!
  /*!
    Get the normalize laser's vector after multiplication with
    wand the scalar length.
  */
  double * GetDirection( double vec[3] );
  //!
  /*!
    The fixed laser length.
  */
  double length;
  //!
  /*!
    Scaled laser direction length (not a unit vector).
  */
  double dir[3]; 
  //!
  /*!
    The end point of the laser.
  */
  double hitXYZ[3]; 
  //! VTK object
  /*!
    A line source for laser.
  */
  vtkLineSource *src;
  //! VTK object
  /*!
    Filter for converting laser from line into tube.
  */
  vtkTubeFilter *filt;
  //! VTK object
  /*!
    Compute normal of the laser.
  */
  vtkPolyDataNormals *norm;
  //! VTK object
  /*!
    Mapping objects with color properties.
  */
  vtkPolyDataMapper *mapper;
  //! VTK object
  /*!
    Translate mapper into vtkActor.
  */
  vtkActor *actor;
  //! Performer object
  /*!
    Performer pfGeode for scenegraph.
  */
  pfGeode *geode;
  //! Performer object
  /*!
    Performer dynamic coordinate systems with pre-loadedtranslated VTK objects. 
  */
  pfDCS *DCS;

};

#endif
