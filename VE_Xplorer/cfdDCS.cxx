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
 * File:          $RCSfile: cfdDCS.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdDCS.h"

#include <Performer/pf/pfDCS.h>

cfdDCS::cfdDCS( float* scale, float* trans, float* rot )
{
   this->_dcs = new pfDCS();
   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
   this->SetScaleArray( scale );
}
/*
cfdDCS::cfdDCS( cfdDCS* x )
{
   this->_dcs = x->_dcs;
   this->SetTranslationArray( x->_translation );
   this->SetRotationArray( x->_rotation );
   this->SetScaleArray( x->_scale );
}
*/
cfdDCS::cfdDCS( void )
{
   this->_dcs = new pfDCS();
}

cfdDCS::~cfdDCS( void )
{
   // If neccesary
   // pfDelete ( this->_dcs );
}

float* cfdDCS::GetTranslationArray( void )
{
   return this->_translation;
}

float* cfdDCS::GetRotationArray( void )
{
   return this->_rotation;
}

float* cfdDCS::GetScaleArray( void )
{
   return this->_scale;
}

void cfdDCS::SetTranslationArray( float* trans )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_translation[ i ] = trans[ i ];
   }

   this->_dcs->setTrans( this->_translation[ 0 ],
                           this->_translation[ 1 ],
                           this->_translation[ 2 ] );
}

void cfdDCS::SetRotationArray( float* rot )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_rotation[ i ] = rot[ i ];
   }

   this->_dcs->setRot( this->_rotation[ 0 ],
                           this->_rotation[ 1 ],
                           this->_rotation[ 2 ] );
}

void cfdDCS::SetScaleArray( float* scale )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_scale[ i ] = scale[ i ];
   }

   this->_dcs->setScale( this->_scale[ 0 ],
                           this->_scale[ 1 ],
                           this->_scale[ 2 ] );
}

pfDCS* cfdDCS::GetPfDCS( void )
{
   return this->_dcs;
}


