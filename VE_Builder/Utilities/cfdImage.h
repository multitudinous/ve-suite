/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdImage.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_IMAGE_H
#define CFD_IMAGE_H

class vtkBMPReader;
class vtkPlaneSource;
class vtkPolyDataMapper;
class vtkTexture;
class vtkActor;
class pfGeoSet;

class VE_XPLORER_EXPORTS cfdImage
{
 public:
   /* Initialize the VTK objects and pipeline.
     When xyz=0 will output the x planes cuts, xyz=1 will output the y planes cuts,
     and xyz=2 will output the z planes cuts.  */

   cfdImage( char * filename, int xyz );

   ~cfdImage( );

   vtkActor * GetActor( );
   bool Update( pfGeoSet *g[4] );

 private:

   int type;             // Direction of cuts. 0 - x planes, 1 - y planes, and 2 - z planes.
   char typeLabel;       // 'X', 'Y', or 'Z'

   vtkBMPReader *bmpReader;
   vtkPlaneSource *plane;
   vtkPolyDataMapper *mapper;
   vtkTexture *texture;
   vtkActor *actor; 
};

#endif
