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
#ifdef _OSG
#include "VE_Xplorer/XplorerHandlers/cfdObjectHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include <vpr/Util/Singleton.h>
#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <osg/Group>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/Array>

/*
#include <osg/Geode>
#include <osg/LineSegment>
#include <osg/Vec3d>
#include <osg/ref_ptr>
#include <osgUtil/IntersectVisitor>
#include <osg/Matrix>

#include <VE_SceneGraph/SceneManager.h>
*/


vprSingletonImp( VE_Xplorer::cfdObjectHandler )

using namespace VE_Xplorer;
    
cfdObjectHandler::cfdObjectHandler( ) 
{
   this->digital.init("VJButton0");
   this->translateDigital.init("VJButton3");
   //selectedGeometry = 0;
   this->rootNode = 0;    
   this->worldNode = 0;
   _active = false; 
}

cfdObjectHandler::~cfdObjectHandler( )
{
   ;
}

void cfdObjectHandler::Initialize( cfdNavigate * navigatorReference )
{
   this->navigator = navigatorReference;
   this->distance = 1000; //* 1000;
   this->laserName = "CFDLaserGeode";
   for (unsigned int i = 0; i < 3; i ++)
   {
      this->LastWandPosition[ i ] = 0;
   }

   this->rootNode = VE_SceneGraph::SceneManager::instance()->GetRootNode();
 
   this->worldNode = this->navigator->worldDCS.get();

}

void SetDataValues( int ValueToSet, int Value)
{
   ValueToSet = Value;
}
////////////////////////////////////////////////
void cfdObjectHandler::ActivateGeometryPicking()
{
   _active = true;
}
/////////////////////////////////////////////////
void cfdObjectHandler::DeactivateGeometryPicking()
{
   _active = false;
}
void cfdObjectHandler::SelectObject()
{
   osg::Vec3d startPoint, endPoint;
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
   this->DrawLine(startPoint, endPoint);
}

void cfdObjectHandler::ProcessHit(osgUtil::IntersectVisitor::HitList listOfHits)
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
   
      if (objectHit._geode.valid())
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
      } 
   }
}
        

//This function currently deletes the existing beam each time it is called and
//add a new beam.  This should be replaced such that it is only called once
//and then a transform is modified for the location.  
void cfdObjectHandler::DrawLine(osg::Vec3d start, osg::Vec3d end)
{
   static osg::Geode* beamGeode;
   static osg::Geometry* beamGeometry;
   
   if (beamGeode != NULL)
   {
      this->rootNode->asGroup()->removeChild(beamGeode);
      //biv -- this disable the drawing of the line
      if(!_active)
      {
         return;
      }
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

//The current implemention uses VJButton0 to select an object then if VJButton3
//is pressed the object is translated with the users motion.  Later the 
//selection method will occur from voice commands and translation will be done
//using gestures from a glove.
void cfdObjectHandler::UpdateObjectHandler()
{
   if ( _active )
   {
      osg::Vec3d startPoint, endPoint;
      this->SetupStartEndPoint(&startPoint, &endPoint);
      this->DrawLine(startPoint, endPoint);

      if (this->digital->getData() == gadget::Digital::TOGGLE_ON)
      {
         this->SelectObject();
      }

      int buttonData = this->translateDigital->getData();
      if ((buttonData == gadget::Digital::ON || buttonData == gadget::Digital::TOGGLE_ON) 
          && this->selectedGeometry != NULL)
      {
         if(buttonData == gadget::Digital::TOGGLE_ON)
         {
	         this->SetWandPosition();
         }
         this->TranslateObject();
      }
   }
}

void cfdObjectHandler::SetupStartEndPoint(osg::Vec3d * startPoint, osg::Vec3d * endPoint)
{
   double * wandPosition  =  this->navigator->GetObjLocation();
   //double * worldPosition = this->navigator->GetWorldLocation();
   double * wandDirection  =  this->navigator->GetDirection();


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

   double * worldRotation  = this->navigator->GetWorldRotation();
   
   osgMat.makeRotate(gmtl::Math::deg2Rad(worldRotation [ 0 ]), 
		     osg::Vec3(0.0, 0.0, 1.0)); 

   *startPoint = osgMat.postMult(*startPoint); 
   *endPoint   = osgMat.postMult(*endPoint);
   
   osgMat.set(this->worldNode->asTransform()->asMatrixTransform()->getMatrix());

   *startPoint = *startPoint *  osgMat;
   *endPoint   = *endPoint * osgMat; 
   

}
void cfdObjectHandler::SetWandPosition()
{
   double * wandPosition = this->navigator->GetObjLocation();
   for (unsigned int i = 0; i < 3; i++)
   {
      this->LastWandPosition[ i ] = wandPosition[ i ];
   }
   vprDEBUG(vesDBG,1) << "reseting wand Position" << std::endl << vprDEBUG_FLUSH;
}

void cfdObjectHandler::TranslateObject()
{
   double * wandPosition = this->navigator->GetObjLocation();
   osg::Vec3d offsetFromLastPosition;
   
   for (int i = 0; i < 3; i++)
   {
      offsetFromLastPosition [ i ]= wandPosition[ i ] - 
	 this->LastWandPosition[ i ];
      this->LastWandPosition[ i ] = wandPosition[ i ];
   }
   
   osg::Matrix osgMat;

   double * worldRotation  = this->navigator->GetWorldRotation();
   
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

osg::MatrixTransform* cfdObjectHandler::getMatrixTransform()
{
   if (this->selectedGeometry->getParents().front()->asGroup() != 0)
   {
      if ( this->selectedGeometry->getParents().front()->getParents().front()
            ->asTransform() != 0 &&
            this->selectedGeometry->getParents().front()->getParents().front()
            ->asTransform()->asMatrixTransform() != 0 &&
            this->selectedGeometry->getParents().front()->getParents().front() !=
            this->worldNode)
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

#endif //_OSG
