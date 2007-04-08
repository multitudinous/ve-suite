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
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef WAND_H
#define WAND_H

/*!\file Wand.h
Wand API
*/
/*!\class VE_XPlorer::Wand
* 
*/
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

#include "VE_Xplorer/SceneGraph/DCS.h"

#include <osgUtil/IntersectVisitor>

namespace osg 
{
   class Geode;
   class Group;
   class Geometry;
   class Vec4f;
   class Vec3f;
   class MatrixTransform;
   class LineSegment;
}

namespace VE_SceneGraph
{
   class DCS;
}

namespace VE_XML
{
   class Command;
}

#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/Device.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Wand : public Device
{
public:
	Wand();
	virtual ~Wand();

   ///Initialize some variables in the class
   void Initialize( void );

	virtual void UpdateNavigation();
	virtual void UpdateSelection();

   ///bool to set the rotation method
   void SetHeadRotationFlag( int );
   ///New function for testing the new VECommand structure
   void SetVECommand( VE_XML::Command* veCommand );
   ///Do not let the user go below the ground plane at 0,0,0 
   void SetSubZeroFlag( int );


   void SelectObject( void );
   void ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits );
   //void DrawLine(osg::Vec3f start, osg::Vec3f end);
   void UpdateObjectHandler( void );
   void SetupStartEndPoint(osg::Vec3f * startPoint, osg::Vec3f * endPoint);
   void TranslateObject( void ); 
   osg::MatrixTransform* getMatrixTransform( void );
   ///Get the current direction of the wand
   double* GetDirection( void );
   double* GetObjLocation( void );
   void UpdateWandLocalDirection( void );
   void UpdateWandLocalLocation( void );     
   void UpdateWandGlobalLocation( void );
   void UpdateDeltaWandPosition( void );
      
      
protected:
   virtual void SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint );
   virtual void DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint );
      
private:
   gadget::DigitalInterface digital[6];
   gadget::DigitalInterface IHdigital[10];
   gadget::DigitalInterface flyThrough[4];
   int buttonData[ 6 ];
   // x, y, and z translation of objects in world coordinates.
   // Variables only used in preFrame
   //double * currentWandDirection;
   int cfdId;
   int cfdIso_value;
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;
   
   //double worldTrans[ 3 ];
   //float worldRot[ 3 ];
   
   /*!
      VR Juggler's wand positional interface.
    */
   //vjPosInterface wand;
   gadget::PositionInterface  wand;
   gadget::PositionInterface  head;
   //! VR Juggler
   /*!
   VR Juggler's vector math function.
   */
   gmtl::Vec3f  vjVec;
   gmtl::Vec3f  LastVec;
   //! VR Juggler
   /*!
      VR Juggler's matrix math function.
    */
   gmtl::Matrix44f vjMat;
   gmtl::Matrix44f vjHeadMat;
   //! Wand object
   /*!
      Location of the wand with respect to the virtual space.
    */
   //double loc[3];
   //! Wand object
   /*!
      Direction of the wand.
    */
   double dir[3];
   //! Virtual environment object(s)
   /*!
      Location of the objects with respect to the virtual space.
    */
   double worldLoc[3];
   //! Cursor object(s)
   /*!
      Location of the cursor with respect to the virtual space.
    */
   double cursorLoc[3];
   //! Data set object(s)
   /*!
      Location with respect to data set (the actual location to interact with data).
    */
   double objLoc[3];
   //! Cursor object(s)
   /*!
      Cursor length.
    */
   float cursorLen;
   
   float translationStepSize;
   float rotationStepSize;
   
   int rotationFlag;
   int subzeroFlag;
   
   // class used to store xml command
   VE_XML::Command* command;
   // data storage for initial world dcs location
   double deltaTrans[ 3 ];
   
   osg::ref_ptr<osg::Geode> selectedGeometry;
   double distance;
   std::string laserName;
   osg::Vec3f LastWandPosition;
   osg::Node* rootNode;
   //osg::Node* worldNode;
   bool _active;
   osg::ref_ptr< osg::Geode > beamGeode;
   osg::ref_ptr< osg::Geometry > beamGeometry;
   osg::ref_ptr< osg::LineSegment > beamLineSegment;
};
}

#endif //WAND_H