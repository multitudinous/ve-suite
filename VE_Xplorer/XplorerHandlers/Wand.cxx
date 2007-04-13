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
#include "VE_Xplorer/XplorerHandlers/Wand.h"

// --- OSG Stuff --- //
#include <osg/LineSegment>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Array>
#include <osg/NodeVisitor>

//C/C++ Libraries
#include <iostream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/FindParentsVisitor.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

// --- VR Juggler Stuff --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

using namespace gmtl;
using namespace gadget;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Wand::Wand() :
   subzeroFlag( 0 ),
   rotationFlag( 1 ),
   distance( 1000 ),
   cursorLen( 1.0f ),
   command( 0 ),
   translationStepSize( 0.25f ),
   rotationStepSize( 1.0f )
{
   wand.init("VJWand");
   head.init("VJHead");
   this->digital[0].init("VJButton0");   // trigger (and top right button) TODO: I think this is unused ?
   this->digital[1].init("VJButton1");   // top left button -- toggle cursor mode: laser, streamlines, box, & arrow
   this->digital[2].init("VJButton2");   // 12 o'clock -- forward navigation
   this->digital[3].init("VJButton3");   // 3 o'clock -- not used at present
   this->digital[4].init("VJButton4");   // 6 o'clock -- reset
   this->digital[5].init("VJButton5");   // 9 o'clock -- exit streamer while loop
   
   this->IHdigital[0].init("VJMovementZ0");  //press "8" for forward navigation
   this->IHdigital[1].init("VJMovementZ1");  //press "2" for backward navigation
   this->IHdigital[2].init("VJMovementX0");  //press "6" for rightward navigation
   this->IHdigital[3].init("VJMovementX1");  //press "4" for leftward navigation
   this->IHdigital[4].init("VJMovementY0");  //press "9" for upward navigation
   this->IHdigital[5].init("VJMovementY1");  //press "7" for downward navigation
   this->IHdigital[6].init("VJRotateX");  //press "right_arrow" for CW rotation
   this->IHdigital[7].init("VJRotateY");  //press "left_arrow" for CCW rotation
                                          //this->IHdigital[8].init("IHVJButton8");  //press "up_arrow" for upward rotation
                                          //this->IHdigital[9].init("IHVJButton9");  //press "down_arrow" for downward rotation
                                          // inputs for flythrough paths   
   beamLineSegment = new osg::LineSegment();
   Initialize();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::Initialize( void )
{
   activeDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
   rootNode = VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode();
   
   for ( int i=0; i<3; i++ )
   {
      this->cursorLoc[i] = 0;//this->loc[i] + this->dir[i]*this->cursorLen;
      this->objLoc[i] = this->cursorLoc[i];
      this->LastVec[i] = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
Wand::~Wand()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateNavigation()
{
   ///Remove the pointer if present
   if ( beamGeode.valid() )
   {
      this->rootNode->asGroup()->removeChild( beamGeode.get() );
   }
   ///Update the wand direction every frame
   UpdateWandLocalDirection();

   this->buttonData[ 1 ] = this->digital[ 1 ]->getData();
   this->buttonData[ 2 ] = this->digital[ 2 ]->getData();
   
   float* tempWorldRot = activeDCS->GetRotationArray();
   float worldRot[ 3 ];
   worldRot[ 0 ] = tempWorldRot[ 0 ];
   worldRot[ 1 ] = tempWorldRot[ 1 ];
   worldRot[ 2 ] = tempWorldRot[ 2 ];
   
	float* tempWorldTrans = activeDCS->GetVETranslationArray();
   float worldTrans[ 3 ];
	worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
	worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
	worldTrans[ 2 ] = -tempWorldTrans[ 2 ];
   
   // This is NOT how we should do things
   // Command should allowed to be null but because we always
   // have to have a command due to our command structure
   // this hack must be in here
   // This should be changed once our complete command structure is
   // in place
   std::string commandType;
   std::string newCommand;
   if ( command )
   {
      commandType = command->GetCommandName();
   }
   else
   {
      commandType = "wait";
      newCommand = "wait";
   }
   
   if ( !commandType.compare( "Navigation_Data" ) )
   {
      VE_XML::DataValuePair* commandData = command->GetDataValuePair( 0 );
      this->cfdIso_value = commandData->GetDataValue();
      newCommand = commandData->GetDataName();
   }
   
   if ( !newCommand.compare( "ROTATE_ABOUT_HEAD" ) )         
   {
      this->SetHeadRotationFlag( this->cfdIso_value );
   }
   else if ( !newCommand.compare( "RESET_NAVIGATION_POSITION" ) )         
   {
      for ( unsigned int i = 0; i < 3; i++ )
	   {
         worldTrans[ i ] = 0.0f;
         worldRot[ i ] = 0.0f;
	   }
   }
   else if ( !newCommand.compare( "CHANGE_TRANSLATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.01' -> 220'
      // the equation is 1/100 * e ^ ( x / 10 ) where 1 < x < 100
      this->translationStepSize = 0.01f * exp( cfdIso_value*0.10f );   
   }
   
   else if ( !newCommand.compare( "CHANGE_ROTATION_STEP_SIZE" ) )         
   {
      // This equation returns a range of ~ 0.00029' -> 1.586' NOTE: These are in degrees
      // This equation is 1 / 750 * ( x / 2 ) ^ 2.2 where 1 < x < 50 
      this->rotationStepSize = (0.001333f) * powf( (cfdIso_value * 0.5f), 2.2f);
   }
   // navigate with buttons now
   if ( ( this->buttonData[1] == gadget::Digital::TOGGLE_ON ) || 
        ( this->buttonData[1] == gadget::Digital::ON ) )
   {
      vprDEBUG(vesDBG,1) << "|\tWand direction :" << this->dir[0] << " : "
                           << this->dir[1] << " : "
                           << this->dir[2]
                           << std::endl << vprDEBUG_FLUSH;
      if ( this->dir[ 0 ] > 0.0f )
      {
         worldRot[ 0 ] -= rotationStepSize;
      }
      else 
      {
         worldRot[ 0 ] += rotationStepSize;
      }
      
      if ( rotationFlag )
      {
         vjHeadMat = head->getData();
         // get juggler Matrix of worldDCS
         // Note:: for pf we are in juggler land
         //        for osg we are in z up land
         Matrix44f worldMat;
         worldMat = activeDCS->GetMat();
         
         gmtl::Point3f jugglerHeadPoint, jugglerHeadPointTemp;
         jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3f >( vjHeadMat );
#ifdef _OSG
         jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
         jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
         jugglerHeadPointTemp[ 2 ] = 0;
#else
         jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[ 0 ];
         jugglerHeadPointTemp[ 1 ] = 0;
         jugglerHeadPointTemp[ 2 ] = jugglerHeadPoint[ 2 ];
#endif
         
         // translate world dcs by distance that the head
         // is away from the origin
         gmtl::Matrix44f transMat = gmtl::makeTrans< gmtl::Matrix44f >( -jugglerHeadPointTemp );
         gmtl::Matrix44f worldMatTrans = transMat * worldMat;
         gmtl::Point3f newJugglerHeadPoint;
         // get the position of the head in the new world space
         // as if the head is on the origin
         gmtl::Point3f newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
         
         // Create rotation matrix and juggler head vector
         gmtl::Matrix44f rotMatTemp;
         if ( this->dir[ 0 ] > 0.0f )
         {
#ifdef _OSG
            gmtl::EulerAngleXYZf worldRotVecTemp(0,0, gmtl::Math::deg2Rad(-rotationStepSize));
#else
            gmtl::EulerAngleXYZf worldRotVecTemp(0, gmtl::Math::deg2Rad(-rotationStepSize), 0);
#endif
            rotMatTemp = gmtl::makeRot< gmtl::Matrix44f >(worldRotVecTemp);
         }
         else 
         {
#ifdef _OSG
            gmtl::EulerAngleXYZf worldRotVecTemp(0,0, gmtl::Math::deg2Rad(rotationStepSize));
#else
            gmtl::EulerAngleXYZf worldRotVecTemp(0, gmtl::Math::deg2Rad(rotationStepSize), 0);
#endif
            rotMatTemp = gmtl::makeRot< gmtl::Matrix44f >(worldRotVecTemp);
         }
         gmtl::Vec4f newGlobalHeadPointVec;
         newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
         newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
         newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];
         // roate the head vector by the rotation increment
         gmtl::Vec4f rotateJugglerHeadVec = rotMatTemp * newGlobalHeadPointVec;
         
         // create translation from new rotated point
         // and add original head off set to the newly found location
         // set world translation accordingly
         worldTrans[0] = -(rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
#ifdef _OSG
         worldTrans[1] = -(rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
#else
         worldTrans[1] = (rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
#endif
      }
   }
   else if ( this->buttonData[2] == gadget::Digital::TOGGLE_ON ||
             this->buttonData[2] == gadget::Digital::ON )
      // Navigation based on current wand direction
   { 
      double* tempWandDir = GetDirection();
      for ( int i=0; i<3; i++ )
      {
         // Update the translation movement for the objects
         // How much object should move
         worldTrans[i] += tempWandDir[i]*this->translationStepSize;
      }
   }
   
   // Set the DCS postion based off of previous 
   // manipulation of the worldTrans array
   for ( unsigned int i = 0; i < 3; i++ )
   {   
      worldTrans[ i ] = -worldTrans[ i ];
   }

   //Do not allow translation below z = 0 plane
   if ( subzeroFlag )
   {
      if ( worldTrans[2] > 0 )
      {
         worldTrans[2] = 0;
      }
   }
   
   activeDCS->SetTranslationArray( worldTrans );
   activeDCS->SetRotationArray( worldRot );   
   vprDEBUG(vesDBG,3) << "|\tEnd Navigate" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateSelection( void )
{
   UpdateObjectHandler();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetHeadRotationFlag( int input )
{
   rotationFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetVECommand( VE_XML::Command* veCommand )
{
   command = veCommand;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SelectObject( void )
{
   osg::Vec3f startPoint, endPoint;
   this->SetupStartEndPoint(&startPoint, &endPoint);

   beamLineSegment->set(startPoint, endPoint);
   
   osgUtil::IntersectVisitor objectBeamIntersectVisitor;
   objectBeamIntersectVisitor.addLineSegment( beamLineSegment.get() );
   
   //Add the IntersectVisitor to the root Node so that all all geometry will be
   //checked and no transforms are done to the Line segement.
   this->rootNode->accept(objectBeamIntersectVisitor);
   
   osgUtil::IntersectVisitor::HitList beamHitList;
   beamHitList = objectBeamIntersectVisitor.getHitList( beamLineSegment.get() );
   
   this->ProcessHit(beamHitList);
}
////////////////////////////////////////////////////////////////////////////////
void Wand::ProcessHit(osgUtil::IntersectVisitor::HitList listOfHits)
{ 
   osgUtil::Hit objectHit;
   this->selectedGeometry = 0;
   
   if ( listOfHits.empty())
   {
      vprDEBUG(vesDBG,1) << "|\tWand::ProcessHit No object selected" << std::endl 
      << vprDEBUG_FLUSH;
      activeDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
      return;
   }

   // Search for first item that is not the laser
   for ( size_t i = 0; i <  listOfHits.size(); ++i )
   {
      objectHit = listOfHits[ i ];
      //std::cout << i << " " <<  objectHit._geode->getName() << std::endl;
      if ( ( objectHit._geode->getName() != this->laserName ) && 
           ( objectHit._geode->getName() != "Root Node" ) ) 
      {
         break;
      }
   }
   //make sure it is good
   if ( !objectHit._geode.valid() )
   {
      return;
   } 
   //now find the id for the cad
   this->selectedGeometry = objectHit._geode;
   VE_SceneGraph::FindParentsVisitor parentVisitor( selectedGeometry.get() );
   osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
   if ( parentNode.valid() )
   {
      std::cout << "|\tObjects has name " 
                  << parentNode->getName() << std::endl
                  << objectHit._geode->getName() << std::endl
                  << objectHit._geode->getParents().front()->getName() << std::endl;
      std::cout << "|\tObjects descriptors " 
                  << parentNode->getDescriptions().at( 1 ) << std::endl;
      activeDCS = dynamic_cast< VE_SceneGraph::DCS* >( parentNode.get() );
   }
   else
   {
      this->selectedGeometry = objectHit._geode;
      std::cout << "|\tObject does not have name parent name " 
                  << objectHit._geode->getParents().front()->getName() 
                  << std::endl;
      activeDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
   }
}
////////////////////////////////////////////////////////////////////////////////
//This function currently deletes the existing beam each time it is called and
//add a new beam.  This should be replaced such that it is only called once
//and then a transform is modified for the location.  
void Wand::DrawLine(osg::Vec3f start, osg::Vec3f end)
{   
   if ( beamGeode.valid() )
   {
      this->rootNode->asGroup()->removeChild( beamGeode.get() );
   }
   
   beamGeode = new osg::Geode();
   beamGeometry = new osg::Geometry();
   beamGeode->addDrawable( beamGeometry.get() );
   beamGeode->setName( this->laserName );
   
   this->rootNode->asGroup()->addChild( beamGeode.get() );
   
   osg::ref_ptr< osg::Vec3Array > beamVertices = new osg::Vec3Array;
   beamVertices->push_back(osg::Vec3(start [ 0 ] - .1, start [ 1 ], 
                                     start [ 2 ])); 
   beamVertices->push_back(osg::Vec3(start [ 0 ] + .1, start [ 1 ], 
                                     start [ 2 ])); 
   beamVertices->push_back(osg::Vec3(end   [ 0 ] + .1, end   [ 1 ],  
                                     end   [ 2 ])); 
   beamVertices->push_back(osg::Vec3(end   [ 0 ] - .1, end   [ 1 ], 
                                     end   [ 2])); 
   beamVertices->push_back(osg::Vec3(start [ 0 ] - .1, start [ 1 ], 
                                     start [ 2 ] + .1) ); 
   beamVertices->push_back(osg::Vec3(start [ 0 ] + .1, start [ 1 ] , 
                                     start [ 2 ] + .1)); 
   beamVertices->push_back(osg::Vec3(end   [ 0 ] + .1, end   [ 1 ], 
                                     end   [ 2 ] + .1)); 
   beamVertices->push_back(osg::Vec3(end   [ 0 ] - .1, end   [ 1 ], 
                                     end   [ 2 ] + .1));
   
   beamGeometry->setVertexArray( beamVertices.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamTop =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamTop->push_back(0);
   beamTop->push_back(1);
   beamTop->push_back(2);
   beamTop->push_back(3);
   beamGeometry->addPrimitiveSet( beamTop.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamBottom =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamBottom->push_back(4);
   beamBottom->push_back(5);
   beamBottom->push_back(6);
   beamBottom->push_back(7);
   beamGeometry->addPrimitiveSet( beamBottom.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamLeft =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamLeft->push_back(0);
   beamLeft->push_back(3);
   beamLeft->push_back(7);
   beamLeft->push_back(4);
   beamGeometry->addPrimitiveSet( beamLeft.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamRight =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamRight->push_back(5);
   beamRight->push_back(6);
   beamRight->push_back(2);
   beamRight->push_back(1);
   beamGeometry->addPrimitiveSet( beamRight.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamBack =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamBack->push_back(1);
   beamBack->push_back(0);
   beamBack->push_back(4);
   beamBack->push_back(5);
   beamGeometry->addPrimitiveSet( beamBack.get() );
   
   osg::ref_ptr< osg::DrawElementsUInt > beamFront =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamFront->push_back(3);
   beamFront->push_back(2);
   beamFront->push_back(6);
   beamFront->push_back(7);
   beamGeometry->addPrimitiveSet(beamFront.get() );
   
   osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
   colors->push_back(osg::Vec4(1.0f, 0.0f, 1.0f, 1.0f) );
   
   osg::ref_ptr< osg::UIntArray > cfdColorIndexArray = new osg::UIntArray();
   cfdColorIndexArray->push_back(0);
   
   beamGeometry->setColorArray(colors.get() );
   beamGeometry->setColorIndices(cfdColorIndexArray.get());
   beamGeometry->setColorBinding(osg::Geometry::BIND_OVERALL) ;
   
}   
////////////////////////////////////////////////////////////////////////////////
//The current implemention uses VJButton0 to select an object then if VJButton3
//is pressed the object is translated with the users motion.  Later the 
//selection method will occur from voice commands and translation will be done
//using gestures from a glove.
void Wand::UpdateObjectHandler( void )
{
   vprDEBUG(vesDBG,3) << "|\tStart Wand::UpdateObjectHandler" 
                        << std::endl << vprDEBUG_FLUSH;
   //Update the juggler location of the wand
   UpdateWandLocalDirection();
   UpdateWandGlobalLocation();
   UpdateDeltaWandPosition();
   
   //Now draw the new line location and setup the data for the hit list pointer
   osg::Vec3f startPoint, endPoint;
   this->SetupStartEndPoint(&startPoint, &endPoint);
   this->DrawLine(startPoint, endPoint);
   
   //Now select and object based on the new wand location
   if ( this->digital[ 0 ]->getData() == gadget::Digital::TOGGLE_ON )
   {
      SelectObject();
      //Set delta back to 0 so that it is not moved by the old delta from the 
      //previous frame
      UpdateDeltaWandPosition();
   }
   
   //Now we can move the object if the button
   int buttonData = this->digital[ 3 ]->getData();
   if ( ( ( buttonData == gadget::Digital::ON ) || 
          ( buttonData == gadget::Digital::TOGGLE_ON ) ) &&
        this->selectedGeometry.valid() )
   {
      this->TranslateObject();
   }

   vprDEBUG(vesDBG,3) << "|\tEnd Wand::UpdateObjectHandler" 
                        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetupStartEndPoint(osg::Vec3f * startPoint, osg::Vec3f * endPoint)
{
   double* wandPosition = this->GetObjLocation();
   double* wandDirection = this->GetDirection();
   double wandEndPoint[ 3 ];
   
   for (int i = 0; i < 3; i++)
   {
      wandEndPoint [ i ] = 
               wandPosition [ i ] + (wandDirection [ i ] * this->distance); 
   }
   
   startPoint->set( wandPosition [ 0 ], wandPosition [ 1 ], wandPosition [ 2 ] );
   endPoint->set( wandEndPoint[ 0 ], wandEndPoint[ 1 ], wandEndPoint[ 2 ] );
}
////////////////////////////////////////////////////////////////////////////////
void Wand::TranslateObject()
{
   double* wandPosition = this->GetObjLocation();
   osg::Vec3f offsetFromLastPosition;
   
   for (int i = 0; i < 3; i++)
   {
      offsetFromLastPosition [ i ] = deltaTrans[ i ];
      //            wandPosition[ i ] - this->LastWandPosition[ i ];
      //this->LastWandPosition[ i ] = wandPosition[ i ];
   }
   
   osg::Matrix osgMat;
   
   float * worldRotation  = activeDCS->GetRotationArray();
   
   osgMat.makeRotate(gmtl::Math::deg2Rad(worldRotation [ 0 ]), 
                     osg::Vec3(0.0, 0.0, 1.0)); 
   
   offsetFromLastPosition = osgMat.postMult(offsetFromLastPosition); 
   
   osgMat.makeTranslate(offsetFromLastPosition);
   
   osg::MatrixTransform* myTransform = this->getMatrixTransform();
   
   if (myTransform != NULL)
   {
      myTransform->preMult(osgMat);
   }
   
   this->selectedGeometry->getParents().front()->dirtyBound();
   this->selectedGeometry->getParents().front()->getBound();
}
////////////////////////////////////////////////////////////////////////////////
osg::MatrixTransform* Wand::getMatrixTransform()
{
   if (this->selectedGeometry->getParents().front()->asGroup() != 0)
   {
      if ( this->selectedGeometry->getParents().front()->getParents().front()
           ->asTransform() != 0 &&
           this->selectedGeometry->getParents().front()->getParents().front()
           ->asTransform()->asMatrixTransform() != 0 &&
           this->selectedGeometry->getParents().front()->getParents().front() !=
           activeDCS->asGroup() )
      {
         return this->selectedGeometry->getParents().front()->getParents().front()
         ->asTransform()->asMatrixTransform();
      }
   }
   
   osg::Matrixd myMatrix;
   myMatrix.makeIdentity();
   osg::MatrixTransform* myMatrixTransform = 
      new osg::MatrixTransform(myMatrix);
   myMatrixTransform->addChild(selectedGeometry->getParents().front());
   
   selectedGeometry->getParents().front()->asGroup()
      ->getParents().front()->asGroup()->addChild(myMatrixTransform);
   
   selectedGeometry->getParents().front()->asGroup()
      ->getParents().front()->asGroup()
      ->removeChild(selectedGeometry->getParents().front());
   
   return myMatrixTransform;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandLocalDirection()
{
   // get the normalized direction relative to the juggler frame
   vjVec.set( 0.0f, 0.0f, -1.0f );
   vjMat = wand->getData();
   gmtl::xform( vjVec, vjMat, vjVec );
   gmtl::normalize( vjVec );
   
   // transform from juggler to osg...
   dir[0] =  vjVec[0];
   dir[1] = -vjVec[2];
   dir[2] =  vjVec[1];
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandGlobalLocation( )
{
   //Transform wand point into global space
   // get juggler Matrix of worldDCS
   // Note:: for osg we are in z up land
   gmtl::Point3f loc_temp, osgPointLoc;
   vjMat = wand->getData( );
   gmtl::setTrans(loc_temp,vjMat);
   osgPointLoc[0] =  loc_temp[0];
   osgPointLoc[1] = -loc_temp[2];
   osgPointLoc[2] =  loc_temp[1];
   
   for ( size_t i=0; i<3; i++ )
   {
      this->LastWandPosition[ i ] = objLoc[ i ];
      //cursorLoc[i] = this->loc[i];// + this->dir[i]*this->cursorLen;
      this->objLoc[i] = osgPointLoc[i];
   }
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetObjLocation( )
{
   return this->objLoc;
}
////////////////////////////////////////////////////////////////////////////////
double* Wand::GetDirection( )
{
   return this->dir;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateDeltaWandPosition()
{
   for ( size_t i = 0; i < 3; i++)
   {
      deltaTrans[ i ] = objLoc[ i ] - LastWandPosition[ i ];
   }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetSubZeroFlag( int input )
{
   subzeroFlag = input;
}
