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

//C/C++ Libraries
#include <iostream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
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
Wand::Wand()
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
   flyThrough[0].init("Path_1");
   flyThrough[1].init("Path_2");
   flyThrough[2].init("Path_3");
   flyThrough[3].init("Path_4");
   
   command = 0;
   rotationFlag = 1;
   
   Initialize();
}
////////////////////////////////////////////////////////////////////////////////
void Wand::Initialize( void )
{
   this->worldDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
   rootNode = VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode();
   this->cursorLen = 1.0f;
   this->distance = 1000;
   //this->dObj = 0.05f;
   //this->UpdateDir( );
   //this->UpdateLoc( );
   
   this->translationStepSize = 0.25f;
   this->rotationStepSize = 1.0f;
   this->worldLoc[0] = this->worldLoc[1] = this->worldLoc[2] = 0.0f;
   this->worldTrans[0] = 0;
   this->worldTrans[1] = 0;
   this->worldTrans[2] = 0;
   for ( int i=0; i<3; i++ )
   {
      this->cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
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
   this->buttonData[ 1 ] = this->digital[ 1 ]->getData();
   this->buttonData[ 2 ] = this->digital[ 2 ]->getData();
   
   float* tempWorldRot = this->worldDCS->GetRotationArray();
   this->worldRot[ 0 ] = tempWorldRot[ 0 ];
   this->worldRot[ 1 ] = tempWorldRot[ 1 ];
   this->worldRot[ 2 ] = tempWorldRot[ 2 ];
   
	float* tempWorldTrans = this->worldDCS->GetVETranslationArray();
	this->worldTrans[ 0 ] = -tempWorldTrans[ 0 ];
	this->worldTrans[ 1 ] = -tempWorldTrans[ 1 ];
	this->worldTrans[ 2 ] = -tempWorldTrans[ 2 ];
   
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
         this->worldTrans[ i ] = initialTranslate[ i ];
         this->worldRot[ i ] = initialRotate[ i ];
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
         this->worldRot[ 0 ] -= rotationStepSize;
      }
      else 
      {
         this->worldRot[ 0 ] += rotationStepSize;
      }
      
      if ( rotationFlag )
      {
         vjHeadMat = head->getData();
         // get juggler Matrix of worldDCS
         // Note:: for pf we are in juggler land
         //        for osg we are in z up land
         Matrix44f worldMat;
         worldMat = this->worldDCS->GetMat();
         
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
         this->worldTrans[0] = -(rotateJugglerHeadVec[ 0 ] + jugglerHeadPointTemp[ 0 ] );
#ifdef _OSG
         this->worldTrans[1] = -(rotateJugglerHeadVec[ 1 ] + jugglerHeadPointTemp[ 1 ] );
#else
         this->worldTrans[1] = (rotateJugglerHeadVec[ 2 ] + jugglerHeadPointTemp[ 2 ] );
#endif
      }
   }
   else if ( this->buttonData[2] == gadget::Digital::TOGGLE_ON ||
             this->buttonData[2] == gadget::Digital::ON )
      // Navigation based on current wand direction
   { 
      //this->UpdateDir( );
      double* tempWandDir = GetDirection();
      for ( int i=0; i<3; i++ )
      {
         // Update the translation movement for the objects
         // How much object should move
         this->worldTrans[i] += tempWandDir[i]*this->translationStepSize;
         
         // How much the cursor movement are needed to trace back
         // to the object after each movement of the object
         this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
      }
   }
   
   // Set the DCS postion based off of previous 
   // manipulation of the worldTrans array
   float tempArray[ 3 ];
   for ( unsigned int i = 0; i < 3; i++ )
      tempArray[ i ] = -this->worldTrans[ i ];
   
   vprDEBUG(vesDBG,3) << " Navigate" << std::endl << vprDEBUG_FLUSH;
   this->worldDCS->SetTranslationArray( tempArray );
   this->worldDCS->SetRotationArray( this->worldRot );   
   //this->UpdateLoc( this->worldTrans );
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
/*void Wand::DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint )
{
   ;
}*/
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
void Wand::SetInitialWorldPosition( float* translate, float* rotate, float* scale )
{
   for ( unsigned int i = 0; i < 3; ++i )
   {
      initialTranslate[ i ] = translate[ i ];
      initialRotate[ i ] = rotate[ i ];
   }
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SelectObject( void )
{
   osg::Vec3f startPoint, endPoint;
   this->SetupStartEndPoint(&startPoint, &endPoint);
   
   osg::LineSegment* beamLineSegment = new osg::LineSegment();
   beamLineSegment->set(startPoint, endPoint);
   
   osgUtil::IntersectVisitor objectBeamIntersectVisitor;
   objectBeamIntersectVisitor.addLineSegment(beamLineSegment);
   
   //Add the IntersectVisitor to the root Node so that all all geometry will be
   //checked and no transforms are done to the Line segement.
   this->rootNode->accept(objectBeamIntersectVisitor);
   
   osgUtil::IntersectVisitor::HitList beamHitList;
   beamHitList = objectBeamIntersectVisitor.getHitList(beamLineSegment);
   
   this->ProcessHit(beamHitList);
   //this->DrawLine(startPoint, endPoint);
}
////////////////////////////////////////////////////////////////////////////////
void Wand::ProcessHit(osgUtil::IntersectVisitor::HitList listOfHits)
{ 
   osgUtil::Hit objectHit;
   //this->selectedGeometry = NULL;
   
   if ( listOfHits.empty())
   {
      vprDEBUG(vesDBG,1) << "|\tcfdObjectHandler::ProcessHit No object selected" << std::endl 
      << vprDEBUG_FLUSH;
   }
   else
   {
      for (unsigned int i = 0; i <  listOfHits.size(); i ++)
      {
         objectHit = listOfHits[ i ];
         if (objectHit._geode->getName() != this->laserName)
         {
            break;
         }
      }
      
      /*if (objectHit._geode.valid())
      {
         if (!objectHit._geode->getName().empty())
         {
            if ( objectHit._geode->getName() != this->laserName
                 && objectHit._geode->getName() != "Root Node") 
            {
               this->selectedGeometry = objectHit._geode;
               std::cout << objectHit._geode->getName() << std::endl;
            }
         }
         else
         {
            this->selectedGeometry = objectHit._geode;
            std::cout << objectHit._geode->getParents().front()->getName() 
            << std::endl;
         }
      } */
   }
}
////////////////////////////////////////////////////////////////////////////////
//This function currently deletes the existing beam each time it is called and
//add a new beam.  This should be replaced such that it is only called once
//and then a transform is modified for the location.  
void Wand::DrawLine(osg::Vec3f start, osg::Vec3f end)
{
   static osg::Geode* beamGeode;
   static osg::Geometry* beamGeometry;
   
   if (beamGeode != NULL)
   {
      this->rootNode->asGroup()->removeChild(beamGeode);
      //beamGeode->removeDrawable(beamGeometry);
   }
   
   
   
   beamGeode = new osg::Geode();
   beamGeometry = new osg::Geometry();
   beamGeode->addDrawable(beamGeometry);
   beamGeode->setName(this->laserName);
   
   
   
   this->rootNode->asGroup()->addChild( beamGeode );
   
   osg::Vec3Array* beamVertices = new osg::Vec3Array;
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
   
   beamGeometry->setVertexArray( beamVertices );
   
   
   osg::DrawElementsUInt* beamTop =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamTop->push_back(0);
   beamTop->push_back(1);
   beamTop->push_back(2);
   beamTop->push_back(3);
   beamGeometry->addPrimitiveSet(beamTop);
   
   osg::DrawElementsUInt* beamBottom =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamBottom->push_back(4);
   beamBottom->push_back(5);
   beamBottom->push_back(6);
   beamBottom->push_back(7);
   beamGeometry->addPrimitiveSet(beamBottom);
   
   osg::DrawElementsUInt* beamLeft =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamLeft->push_back(0);
   beamLeft->push_back(3);
   beamLeft->push_back(7);
   beamLeft->push_back(4);
   beamGeometry->addPrimitiveSet(beamLeft);
   
   osg::DrawElementsUInt* beamRight =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamRight->push_back(5);
   beamRight->push_back(6);
   beamRight->push_back(2);
   beamRight->push_back(1);
   beamGeometry->addPrimitiveSet(beamRight);
   
   osg::DrawElementsUInt* beamBack =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamBack->push_back(1);
   beamBack->push_back(0);
   beamBack->push_back(4);
   beamBack->push_back(5);
   beamGeometry->addPrimitiveSet(beamBack);
   
   osg::DrawElementsUInt* beamFront =
      new osg::DrawElementsUInt(osg::PrimitiveSet::QUADS, 0);
   beamFront->push_back(3);
   beamFront->push_back(2);
   beamFront->push_back(6);
   beamFront->push_back(7);
   beamGeometry->addPrimitiveSet(beamFront);
   
   
   osg::Vec4Array* colors = new osg::Vec4Array;
   colors->push_back(osg::Vec4(1.0f, 0.0f, 1.0f, 1.0f) );
   
   osg::ref_ptr< osg::UIntArray > cfdColorIndexArray = new osg::UIntArray();
   cfdColorIndexArray->push_back(0);
   
   beamGeometry->setColorArray(colors);
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
   UpdateWandLocalLocation();
   UpdateWandGlobalLocation();
   UpdateDeltaWandPosition();
   
   osg::Vec3f startPoint, endPoint;
   this->SetupStartEndPoint(&startPoint, &endPoint);
   this->DrawLine(startPoint, endPoint);
   
   if (this->digital[ 0 ]->getData() == gadget::Digital::TOGGLE_ON)
   {
      this->SelectObject();
   }
   
   int buttonData = this->digital[ 0 ]->getData();
   if ((buttonData == gadget::Digital::ON || buttonData == gadget::Digital::TOGGLE_ON) 
       && this->selectedGeometry != NULL)
   {
      //if(buttonData == gadget::Digital::TOGGLE_ON)
      {
         //this->SetWandPosition();
      }
      this->TranslateObject();
   }

   vprDEBUG(vesDBG,3) << "|\tEnd Wand::UpdateObjectHandler" 
                        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void Wand::SetupStartEndPoint(osg::Vec3f * startPoint, osg::Vec3f * endPoint)
{
   double* wandPosition  =  this->GetObjLocation();
   //double * worldPosition = this->navigator->GetWorldLocation();
   double* wandDirection  =  this->GetDirection();
   
   
   for (int i = 0; i < 3; i++)
   {
      wandDirection [ i ] =  wandPosition [ i ] + 
      (wandDirection [ i ] * this->distance); 
   }
   
   startPoint 
      ->set(wandPosition [ 0 ], wandPosition [ 1 ], wandPosition [ 2 ]);
   endPoint 
      ->set(wandDirection [ 0 ], wandDirection[ 1 ], wandDirection [ 2 ]);
   
   osg::Matrix osgMat;
   
   float * worldRotation  = worldDCS->GetRotationArray();
   
   osgMat.makeRotate(gmtl::Math::deg2Rad(worldRotation [ 0 ]), 
                     osg::Vec3(0.0, 0.0, 1.0)); 
   
   *startPoint = osgMat.postMult(*startPoint); 
   *endPoint   = osgMat.postMult(*endPoint);
   
   osgMat.setRotate( this->worldDCS->getAttitude() );
   osgMat.setTrans( this->worldDCS->getPosition() );
   
   *startPoint = *startPoint *  osgMat;
   *endPoint   = *endPoint * osgMat;
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
   
   float * worldRotation  = worldDCS->GetRotationArray();
   
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
           this->worldDCS->asGroup() )
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
   vjMat = wand->getData( );
   vjVec = gmtl::xform( vjVec,vjMat,vjVec);
   gmtl::normalize(vjVec);
   
   // transform from juggler to performer...
   dir[0] =  vjVec[0];
   dir[1] = -vjVec[2];
   dir[2] =  vjVec[1];
}
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandLocalLocation( void )      
{
   // get the location relative to the juggler frame
   gmtl::Vec3f loc_temp;
   gmtl::setTrans(loc_temp,vjMat);
   
   // transform from juggler to performer...
   loc[0] =  loc_temp[0];
   loc[1] = -loc_temp[2];
   loc[2] =  loc_temp[1];
}  
////////////////////////////////////////////////////////////////////////////////
void Wand::UpdateWandGlobalLocation( )
{
   //this->UpdateDir( );
   //this->UpdateLoc( );
   double cursorLoc[ 3 ];
   float* worldDCSLocation = worldDCS->GetVETranslationArray();
   
   for ( size_t i=0; i<3; i++ )
   {
      this->LastWandPosition[ i ] = objLoc[ i ];
      cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
      //this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
      this->objLoc[i] = cursorLoc[i] + worldDCSLocation[i];
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
