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
   public:
      cfdObjectHandler( );
      ~cfdObjectHandler( );
      vprSingletonHeader( cfdObjectHandler );   

      void Initialize(cfdNavigate* );
      void SetDataValues( int, int );
      void UpdateObjectHandler();
      void DrawLine(osg::Vec3f, osg::Vec3f);
      void ChangeColor(osg::Vec3f);
      void ProcessHit(osgUtil::IntersectVisitor::HitList);
      void ActivateGeometryPicking();
      void DeactivateGeometryPicking();
      cfdNavigate* navigator;
   
   private:

      void SelectObject();
      void ChangeColor (osg::Vec4f);
      void ChangeColor (osg::Geometry *, osg::Vec4f); 
      gadget::DigitalInterface digital;
      osg::ref_ptr<osg::Geode> selectedGeometry;
      double distance;
      std::string laserName;
      osg::Vec3f LastWandPosition;
      osg::MatrixTransform* getMatrixTransform();
      void TranslateObject();
      gadget::DigitalInterface translateDigital;
      osg::Node* rootNode;
      osg::Node* worldNode;
      void SetupStartEndPoint(osg::Vec3f *, osg::Vec3f *);
      void SetWandPosition();
      bool _active;
};

}
#endif //CFD_OJECTHANDLER_H
#endif // _OSG
#endif // VE_PATENTED
