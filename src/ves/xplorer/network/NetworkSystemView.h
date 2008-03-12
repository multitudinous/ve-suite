/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef NETWORK_SYSTEM_VIEW_H
#define NETWORK_SYSTEM_VIEW_H
#include <ves/xplorer/GlobalBase.h>

#include <ves/open/xml/model/SystemPtr.h>
#include <string>
#include <map>
#include <osg/ref_ptr>
namespace osg
{
class Group;
class AutoTransform;
}

namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS NetworkSystemView : public ves::xplorer::GlobalBase
{
public:
    NetworkSystemView();
    NetworkSystemView( std::string );
    NetworkSystemView( const NetworkSystemView& );

    virtual ~NetworkSystemView( void );
    NetworkSystemView& operator=( const NetworkSystemView& );

    void UpdateCommand( void )
    {
        ;
    }
    //osg::ref_ptr< osg::Group > DrawNetwork( void );
	osg::ref_ptr< osg::Group > DrawNetwork( std::string netId );
	void LoadVESData( std::string xmlNetwork );
	void ParseSystem( ves::open::xml::model::SystemPtr system );

private:
    std::string network;
    osg::ref_ptr<osg::AutoTransform> worldTranslate;
	std::map< std::string, ves::open::xml::model::SystemPtr > systems;

};
}
}
}
#endif
