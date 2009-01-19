/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/network/cfdVEAvailModules.h>
#include <ves/xplorer/network/cfdVEPluginLoader.h>
#include <iostream>

#include <fstream>
#include <sstream>

using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////////
cfdVEAvailModules::cfdVEAvailModules()
{
    pl_loader = new cfdVEPluginLoader();
    LoadModules();
}
////////////////////////////////////////////////////////////////////////////////
cfdVEAvailModules::~cfdVEAvailModules()
{
    delete pl_loader;
}
////////////////////////////////////////////////////////////////////////////////
bool cfdVEAvailModules::LoadModules()
{
    pl_loader->ScanAndLoad();
    return true;
}
////////////////////////////////////////////////////////////////////////////////
cfdVEPluginLoader* cfdVEAvailModules::GetLoader()
{
    return pl_loader;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVEAvailModules::ResetPluginLoader()
{
#ifndef WIN32
//This define is required because on windows when dll's are unloaded the
//memory allocated by the dll's is also unloaded. This causes problems with
//OSG because OSG has ref_ptr's still pointing to memory allocated by the dll
//a few frames AFTER the dll has been unloaded. This is not a problem
//on other operating systems due to the way plugable modules are handled
//by the operating system. For more information on this bug please see
//the thread on the osg list by Paul Martz on April 19th, 2008
//titled: Clearing RenderLeaf ref_ptrs
//If a better solution for managing this problem can be created it should be
//investigated as this solution is a major hack.
    delete pl_loader;
#endif
    pl_loader = new cfdVEPluginLoader();
    LoadModules();
}

