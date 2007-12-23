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
 * File:          $RCSfile: cfdVEBaseClass.h,v $
 * Date modified: $Date: 2005-09-23 12:19:05 -0500 (Fri, 23 Sep 2005) $
 * Version:       $Rev: 3081 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_Int_Stove_EconGraphicalPlugin_H
#define VE_Int_Stove_EconGraphicalPlugin_H

#include "VE_Xplorer/GraphicalPlugin/cfdVEBaseClass.h"

namespace VE_SceneGraph
{
   class Group;
   class Geode;
}


class VE_USER_PLUGIN_EXPORTS VEInt_Stove_EconGraphicalPlugin: public VE_Xplorer::cfdVEBaseClass 
{
public:
   VEInt_Stove_EconGraphicalPlugin( void );
   ~VEInt_Stove_EconGraphicalPlugin( void );

   virtual void InitializeNode( VE_SceneGraph::DCS* );
private:
};

extern "C"
{
VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
{
   return new VEInt_Stove_EconGraphicalPlugin();
}
}


#endif
   

