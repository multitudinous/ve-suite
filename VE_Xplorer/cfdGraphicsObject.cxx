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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdGraphicsObject.h"

#include "cfdGeode.h"
#include "cfdGroup.h"

// constructor
cfdGraphicsObject::cfdGraphicsObject( void )
{
   geode = 0;
   parentNode = 0;
   worldNode = 0;
   type = OTHER;
   actor = 0;
}

// destructor
cfdGraphicsObject::~cfdGraphicsObject( void )
{
   if ( geode )
   {
      delete geode;
      geode = 0;
   }
   type = OTHER;
}

// copy constructor
cfdGraphicsObject::cfdGraphicsObject( const cfdGraphicsObject& input )
{
   // do nothing yet
}

// equal operator
cfdGraphicsObject& cfdGraphicsObject::operator=( const cfdGraphicsObject& input )
{
   // do nothing yet
   if ( this != &input )
   {
   }
   return *this;
}

// Set parent node to add "graphics node" to
void cfdGraphicsObject::SetParentNode( cfdGroup* input )
{
   this->parentNode = input;
}

// node the parent node will be added to
void cfdGraphicsObject::SetWorldNode( cfdGroup* input )
{
   this->worldNode = input;
}

// add "child node" to scene graph
void cfdGraphicsObject::AddGraphicsObjectToSceneGraph( void )
{
   if ( this->type == CLASSIC )
   {
      this->parentNode->AddChild( this->geode );
   }
/*         if ( this->dataList[ i ]->GetGeodeFlag() )
         {
            vprDEBUG(vprDBG_ALL,2) << " have geode flag"
                                   << std::endl << vprDEBUG_FLUSH;

            if ( this->_worldDCS->SearchChild( this->dataList[ i ]->GetDCS() ) < 0 )
            {
               vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                                   << std::endl << vprDEBUG_FLUSH;
               this->_worldDCS->AddChild( this->dataList[ i ]->GetDCS() );
            }

            if ( this->dataList[ i ]->GetcfdGeode() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add geode to sg"
                                      << std::endl << vprDEBUG_FLUSH;
               // Add steady state geodes to the scene graph
               this->dataList[ i ]->AddcfdGeodeToDCS();
            }
            else if ( this->dataList[ i ]->GetSequence() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add viz object "
                                      << i << " to sequence"
                                      << std::endl << vprDEBUG_FLUSH;

               {
                  this->dataList[ i ]->GetSequence()->AddToSequence(
                                          this->dataList[ i ]->GetObjectType() );
                  if ( this->dataList[ i ]->GetDCS()->SearchChild(
                        this->dataList[ i ]->GetSequence()->GetSequence() ) < 0 )
                  {
                     this->dataList[ i ]->GetDCS()->AddChild( 
                           this->dataList[ i ]->GetSequence()->GetSequence() );
                  }
               }
            }
            vprDEBUG(vprDBG_ALL,2) << " End Update Loop"
                                   << std::endl << vprDEBUG_FLUSH;

            // Resetting these variables is very important
            this->dataList[ i ]->SetUpdateFlag( false );
            this->dataList[ i ]->SetGeodeFlag( false );
            this->actorsAreReady = false;
         }else if(this->dataList.at(i)->GetTransientGeodeFlag()){
            this->dataList.at(i)->SetSequence( this->_activeDataSet->GetAnimation() );
            if ( this->_worldDCS->SearchChild( this->dataList[ i ]->GetSequence()->GetSequence() ) < 0 )
            {
               vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                                   << std::endl << vprDEBUG_FLUSH;
               this->_worldDCS->AddChild( this->dataList[ i ]->GetSequence()->GetSequence() );
            }

            this->dataList.at(i)->AddGeodesToSequence();
            this->dataList.at(i)->SetTransientGeodeFlag(false);
            this->dataList.at(i)->SetSequence( 0 );
            this->transientBusy = false;
         }
*/
}

// set type of viz: trans, classic, texture
void cfdGraphicsObject::SetTypeOfViz( VizType x )
{
   this->type = x;
}

// set actor for classic and trans viz objects
void cfdGraphicsObject::SetActor( vtkActor* input )
{
   this->actor = input;
}
