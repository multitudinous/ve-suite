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
* Date modified: $Date: 2006-12-19 22:02:50 -0600 (Tue, 19 Dec 2006) $
* Version:       $Rev: 6352 $
* Author:        $Author: mccdo $
* Id:            $Id: cfdFILE.cxx 6352 2006-12-20 04:02:50Z mccdo $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerNetwork/NetworkSystemView.h"

#include <osg/Group>

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::NetworkSystemView( const NetworkSystemView& input )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView::~NetworkSystemView( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
NetworkSystemView& NetworkSystemView::operator=( const NetworkSystemView& input )
{
   if ( this != &input )
   {
      ;
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkSystemView::SetNetwork( std::string network )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr< osg::Group > NetworkSystemView::DrawNetwork( void )
{
   return new osg::Group();
}
