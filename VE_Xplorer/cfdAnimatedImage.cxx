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
 * File:          $RCSfile: cfdAnimatedImage.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdAnimatedImage.h"
#include "cfdReadParam.h"
#include "cfdImage.h"

#ifdef _CFDCOMMANDARRAY
#include "cfdApp.h"
#endif //_CFDCOMMANDARRAY

#include <Performer/pf/pfDCS.h>

#include "cfdSequence.h"

#include <vpr/Util/Debug.h>

cfdAnimatedImage::cfdAnimatedImage( char *basename, int frames,
                                    int ex_x, int ex_y, int dim,
                                    double *origin, double *spacing )
{
  unsigned int i;
  char filename[250];
  cfdImage* im;
  _frames = frames;
  //  _which_frame = 0;

  _images.clear();

   for(i=0; i<(unsigned)frames; i++) 
   {
      sprintf(filename, "%s%02d.lic", basename, i);

      im = new cfdImage(filename, ex_x, ex_y, dim, origin, spacing);

      _images.push_back(im);
   }

   this->sequence = new cfdSequence();
}

cfdAnimatedImage::cfdAnimatedImage( cfdReadParam* param )
{
   unsigned int i;
   char filename[250];
   cfdImage* im;
   _frames = param->frames;
   //  _which_frame = 0;

   _images.clear();

   for(i=0; i<(unsigned)_frames; i++) 
   {
      sprintf(filename, "%s%02d.lic", param->basename, i);

      im = new cfdImage(filename, param->ex_x, param->ex_y, param->dim, param->origin, param->spacing);

      _images.push_back(im);
   }

   this->SetTranslationArray( param->imageTrans );
   this->SetRotationArray( param->imageRot );
   this->SetScaleArray( param->imageScale );

   this->sequence = new cfdSequence();

   this->GetPfDCS()->addChild( (pfNode*)this->sequence );
}

cfdAnimatedImage::~cfdAnimatedImage()
{
   unsigned int i;

   for (i=0; i<_images.size(); i++)
      delete _images[i];

   _images.clear();

   this->ClearSequence();
   pfDelete( this->sequence );
}

#ifdef _CFDCOMMANDARRAY
bool cfdAnimatedImage::CheckCommandId( cfdApp * _cfdApp )
{
   return false;
}

void cfdAnimatedImage::UpdateCommand()
{
   cerr << "doing nothing in cfdAnimatedImage::UpdateCommand()" << endl;
}
#endif //_CFDCOMMANDARRAY

void cfdAnimatedImage::Update( void )
{
   int i;
   for (i=0; i<_frames; i++)
   {
      this->actor = _images[i]->GetActor();
  
      this->CreateGeode();
   }

   this->updateFlag = true;
   this->addGeode = true;      
}
