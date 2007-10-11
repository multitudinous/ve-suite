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
#include <ves/xplorer/event/cfdAnimatedImage.h>
#include <ves/xplorer/event/cfdReadParam.h>
#include <ves/xplorer/event/cfdImage.h>
#include <ves/xplorer/event/cfdEnum.h>
#include <ves/xplorer/event/cfdCommandArray.h>

#include <ves/xplorer/event/cfdDebug.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// Fix need to add the new style read param to this class
// tak code out of cfdReadPAram to this function
cfdAnimatedImage::cfdAnimatedImage( std::string basename, int frames,
                                    int ex_x, int ex_y, int dim,
                                    double *origin, double *spacing )
{
   _readParam = new cfdReadParam();
   //CreateObjects();
  unsigned int i;
  //char* filename;//[250];
  frames = frames;
  //  _which_frame = 0;

  _images.clear();

   for(i=0; i<(unsigned)frames; i++) 
   {
      //sprintf(filename, "%s%02d.lic", basename, i);
      std::ostringstream dirStringStream;
	  dirStringStream << basename << std::setw(2) << std::setfill('0') << i << ".lic";
      std::string dirString = dirStringStream.str();
      //filename = (char*)dirString.c_str();

      cfdImage* im = new cfdImage((char*)dirString.c_str(), ex_x, ex_y, dim, origin, spacing);

      _images.push_back(im);
   }

   //this->_sequence = new cfdTempAnimation();
   this->_dcs = new VE_SceneGraph::DCS();
}

cfdAnimatedImage::cfdAnimatedImage( std::string param )
{
   unsigned int i;
   //char* filename;//[250];
   //frames = param->frames;
   //  _which_frame = 0;
   frames = 0;
   _readParam = new cfdReadParam();
   _param = param;
   CreateObjects();

   if ( frames == 0 )
      return;

   _images.clear();
   
   // Nedd to fix this
   // probably create new function
   for(i=0; i<(unsigned)frames; i++) 
   {
      //sprintf(filename, "%s%02d.lic", basename, i);
      std::ostringstream dirStringStream;
	  dirStringStream << basename << std::setw(2) << std::setfill('0') << i << ".lic";
      std::string dirString = dirStringStream.str();
      //filename = (char*)dirString.c_str();

      cfdImage* im = new cfdImage((char*)dirString.c_str(), ex_x,ex_y, dim, origin, spacing);

      _images.push_back(im);
   }

   this->_dcs = new VE_SceneGraph::DCS();
   this->_dcs->SetTranslationArray( imageTrans );
   this->_dcs->SetRotationArray( imageRot );
   this->_dcs->SetScaleArray( imageScale );

   //this->_sequence = new cfdTempAnimation();
   
   // Inhereted function from DCS
   //this->_dcs->AddChild( (cfdNode*)this->_sequence->GetSequence() );
}

cfdAnimatedImage::~cfdAnimatedImage()
{
   unsigned int i;

   for (i=0; i<_images.size(); i++)
      delete _images[i];

   _images.clear();

   if ( frames != 0 )
   {
      //this->_sequence->ClearSequence();
      //delete this->_sequence;
      //delete this->_dcs;
   }
}

bool cfdAnimatedImage::CheckCommandId( cfdCommandArray* commandArray )
{
   return false;
}

void cfdAnimatedImage::UpdateCommand()
{
   std::cerr << "doing nothing in cfdAnimatedImage::UpdateCommand()" << std::endl;
}

void cfdAnimatedImage::Update( void )
{
   int i;
   for (i=0; i<frames; i++)
   {
      //this->actor = _images[i]->GetActor();
  
      //this->_sequence->CreateGeodeVector( this->actor );
   }

   this->updateFlag = true;     
}

void cfdAnimatedImage::CreateObjects( void )
{
   int numObjects;
   char textLine[ 256 ];
   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 
   input.getline( textLine, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vesDBG,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( textLine, 256 );   //skip past remainder of line
      if ( id == 12 )
      {
         input >> basename;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> frames;
         input >> ex_x;
         input >> ex_y;
         input >> dim;
         input.getline( textLine, 256 );
         input >> origin[0];
         input >> origin[1];
         input >> origin[2];
         input >> spacing[0];
         input >> spacing[1];
         input >> spacing[2];
         input.getline( textLine, 256 );

         //this->_readParam->read_pf_DCS_parameters( input, this->imageScale, this->imageTrans, this->imageRot );
      }
      else
      {
         // Skip past block
         //_readParam->ContinueRead( input, id );
      }
   }
}
