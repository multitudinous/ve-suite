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
 * File:          $RCSfile: cfdAnimatedImage.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ANIMATED_IMAGE_H
#define CFD_ANIMATED_IMAGE_H

#include "cfdObjects.h"

class vtkPolyDataMapper;
class vtkPolyData;
class vtkGlyph3D;
class vtkSphereSource;
class cfdImage;
class cfdReadParam;
class cfdCommandArray;
class cfdDCS;

class WXPLUGIN_DECLSPEC cfdAnimatedImage : public cfdObjects
{
   public:
      cfdAnimatedImage( char *basename, int frames,
                    int ex_x, int ex_y, int dim, 
                    double *origin, double *spacing );

      cfdAnimatedImage( char* );
  
      ~cfdAnimatedImage();
  
      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdCommandArray* commandArray );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();

      // update the actor
      virtual void Update( void );
  
      void CreateObjects( void );

      std::vector< cfdImage* > _images;

   private:
      cfdDCS* _dcs;
      char basename[256];
      int frames;
      int ex_x, ex_y;
      int dim;
      double origin[3];
      double spacing[3];
      float imageScale[ 3 ];
      float imageTrans[ 3 ];
      float imageRot[ 3 ];
      char* _param;
      cfdReadParam* _readParam;
      //int _which_frame;
};

#endif
