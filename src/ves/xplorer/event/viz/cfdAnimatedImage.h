/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ANIMATED_IMAGE_H
#define CFD_ANIMATED_IMAGE_H
/*!\file cfdAnimatedImage.h
cfdAnimatedImage API
*/
/*!\class VE_Xplorer::cfdAnimatedImage
* 
*/
#include <ves/xplorer/event/viz/cfdObjects.h>

#include <ves/xplorer/scenegraph/DCS.h>

class vtkPolyDataMapper;
class vtkPolyData;
class vtkGlyph3D;
class vtkSphereSource;

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
   class cfdImage;
   class cfdReadParam;
   class cfdCommandArray;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
   class DCS;
}
}
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdAnimatedImage : public cfdObjects
   {
      public:
         ///Fix needed to add the new style read param to this class
         ///takes code out of cfdReadParam to this function
         ///\param basename
         ///\param frames
         ///\param ex_x
         ///\param ex_y
         ///\param dim
         ///\param origin
         ///\param spacing
         cfdAnimatedImage( std::string basename, int frames,
                    int ex_x, int ex_y, int dim, 
                    double *origin, double *spacing );
         ///Reads in parameters for animated image
         ///\param param
         cfdAnimatedImage( std::string param);
         ///Destructor
         virtual ~cfdAnimatedImage();
  
         ///compare VjObs_i commandArray with its child's value
         ///\param commandArray
         virtual bool CheckCommandId( cfdCommandArray* commandArray );

         ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();

         ///Update the actor
         virtual void Update( void );
         ///Create objects
         void CreateObjects( void );

         std::vector< cfdImage* > _images;///<The vector of images.

      private:
			osg::ref_ptr< ves::xplorer::scenegraph::DCS > _dcs;///<reference pointer to the dcs
         char basename[256];///<Holds basename input.
         int frames;///<Number of frames.
         int ex_x, ex_y;///<ex_x and ex_y.
         int dim;///<dimension.
         double origin[3];///<Defines origin.
         double spacing[3];///<Spacing.
         double imageScale[ 3 ];///<Scaling of Image
         double imageTrans[ 3 ];///<Translation of image.
         double imageRot[ 3 ];///<Rotation of image.
         std::string _param;///<The string storing parameters.
         //std::string _param
         cfdReadParam* _readParam;///<Read out parameters.
         //int _which_frame;
   };
}
#endif
