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
 * File:          $RCSfile: cfdImage.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_IMAGE_H
#define CFD_IMAGE_H

#include "cfdObjects.h"

class vtkBMPReader;
class vtkImageReader;
class vtkPlaneSource;
class vtkPolyDataMapper;
class vtkTexture;

#ifdef _CFDCOMMANDARRAY
class cfdApp;
#endif //_CFDCOMMANDARRAY

class cfdImage : public cfdObjects
{
 public:
   cfdImage( char * filename, double * position, int xyz );

   cfdImage( char * filename, int ex_x, int ex_y, int dim,
             double *origin, double *spacing );

   ~cfdImage( );

#ifdef _CFDCOMMANDARRAY
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdApp * _cfdApp );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

   // update the actor
   virtual void Update( void );

   vtkActor * GetActor( );

 private:

   int type;         // Direction: 0=X-plane, 1=Y-plane, and 2=Z-plane.
   char typeLabel;   // 'X', 'Y', or 'Z'

   vtkBMPReader *bmpReader;
   vtkImageReader *imgReader;
   vtkPlaneSource *plane;
   vtkPolyDataMapper *mapper;
   vtkTexture *texture;
};

#endif
