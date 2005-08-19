/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
#include "VE_Conductor/Framework/GeometryDataManager.h"
#include "VE_Conductor/Framework/GeometryDataBuffer.h"

GeometryDataManager* GeometryDataManager::myinstance =0;

GeometryDataManager& GeometryDataManager::getInstance()
{
   if(myinstance == 0)
   {
      myinstance = new GeometryDataManager();

   }
   return *myinstance;

}

GeometryDataManager::~GeometryDataManager()
{
   delete _GeometryDataBuffer;
   _GeometryDataBuffer =0;
}

GeometryDataBuffer* GeometryDataManager::GetGeometryDataBuffer()
{
   if(!_GeometryDataBuffer)
   {
      _GeometryDataBuffer = new GeometryDataBuffer;

   }

   return _GeometryDataBuffer;
}
