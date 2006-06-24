/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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

#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"

class vtkBMPReader;
class vtkImageReader;
class vtkPlaneSource;
class vtkPolyDataMapper;
class vtkTexture;
class vtkActor;
namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdReadParam;
}
#include "VE_Installer/include/VEConfig.h"
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdImage : public cfdObjects
   {
      public:
         cfdImage( std::string );

         cfdImage( std::string filename, int ex_x, int ex_y, int dim,
             double *origin, double *spacing );

         ~cfdImage( );

         // compare VjObs_i commandArray with its child's value
         virtual bool CheckCommandId( cfdCommandArray* commandArray );

         // in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();

         // update the actor
         virtual void Update( void );

         vtkActor* GetActor( void );

         void CreateObjects( void );

      private:
         int type;         // Direction: 0=X-plane, 1=Y-plane, and 2=Z-plane.
         char typeLabel;   // 'X', 'Y', or 'Z'

         vtkBMPReader *bmpReader;
         vtkImageReader *imgReader;
         vtkPlaneSource *plane;
         vtkPolyDataMapper *mapper;
         vtkTexture *texture;
         //char  bmpFileName[ 100 ];
         std::string bmpFileName;
         double bmpPosition[ 3 ];
         int bmpOrientation;  // 0=X-plane, 1=Y-plane, and 2=Z-plane.
         std::string _param;
         VE_Xplorer::cfdReadParam* _readParam;
   };
}
#endif
