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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#ifdef _OSG

#ifndef CFD_OBJECTHANDLER_H
#define CFD_OBJECTHANDLER_H

/*!\file cfdObjectHandler.h
cfdObjectHandler API
*/
/*!\class VE_Xplorer::cfdObjectHandler
* 
*/

#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

#include <osg/ref_ptr>
#include <osgUtil/IntersectVisitor>
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class cfdNavigate;
}

namespace osg 
{
  class Geode;
  class Group;
  class Geometry;
  class Vec4f;
  class Vec3f;
  class MatrixTransform;
}

namespace osgUtil
{
   class IntersectVisitor;
   class Hit;
   class HitList;
}

namespace IntersectVisitor
{
  class HitList;
}

//! Navigation tracker
/*!
  A class to track the wand location, object translation,
  and virtual cursor location in virtual environment.
*/

namespace VE_Xplorer 
{
class VE_XPLORER_EXPORTS cfdObjectHandler
{
private:

   ///Constructor.
   cfdObjectHandler( );

   ///Destructor.
   ~cfdObjectHandler( );

   ///Not sure what this does
   vprSingletonHeader( cfdObjectHandler );   
public:

   ///Initializes the reference point for the navigation.
   ///\param navigationReference
   void Initialize(cfdNavigate* navigationReference );

   ///Setting up the data values.
   ///\param ValueToSet
   ///\param Value
   void SetDataValues( int ValueToSet, int Value );

   ///Used to select an object and translate with user motion with wand.
   void UpdateObjectHandler();

   ///Deletes existing beam aand adds new one.
   ///\param start
   ///\param end
   void DrawLine(osg::Vec3f start , osg::Vec3f end);

   ///not sure if this is needed?
   void ChangeColor(osg::Vec3f);

   ///Process whether an object is hit?
   ///\param listOfHits
   void ProcessHit(osgUtil::IntersectVisitor::HitList listOfHits);

   ///Activate geometry to allow picking.
   void ActivateGeometryPicking();

   ///Deactivate geometry to not allow picking.
   void DeactivateGeometryPicking();

   cfdNavigate* navigator;///<navigator reference 

private:

   ///Select object.
   void SelectObject();
   
   ///Not sure if used?
   void ChangeColor (osg::Vec4f);

   ///Not sure if used?
   void ChangeColor (osg::Geometry *, osg::Vec4f); 

   gadget::DigitalInterface digital;///<gadgeteer digital interface.
   osg::ref_ptr<osg::Geode> selectedGeometry;///<select geometry.
   double distance;///<Distance which is initialized.
   std::string laserName;///<name of laser.
   osg::Vec3f LastWandPosition;///<wand position vector.

   ///performs matrix tranformation.
   osg::MatrixTransform* getMatrixTransform();

   ///Performs translation of object.
   void TranslateObject();

   gadget::DigitalInterface translateDigital;///<gadgeteer translation??
   osg::Node* rootNode;///<root node for osg.
   osg::Node* worldNode;///<world node for osg.

   ///Assigns start and ending points of wand.
   ///\param *startPoint
   ///\param *endPoint
   void SetupStartEndPoint(osg::Vec3f * startPoint, osg::Vec3f * endPoint );

   ///Resets wand position.
   void SetWandPosition();

   ///active flag
   bool _active;
};

}
#endif //CFD_OJECTHANDLER_H
#endif // _OSG
#endif // VE_PATENTED
