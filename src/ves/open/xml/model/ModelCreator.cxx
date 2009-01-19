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
#include <ves/open/xml/model/ModelCreator.h>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/Tag.h>

using namespace ves::open::xml::model;
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr ModelCreator::CreateNewXMLObject( const std::string& objectType )
{
    ves::open::xml::XMLObjectPtr tmp = ves::open::xml::XMLObjectPtr();
    if( objectType == "Link" )
    {
        tmp = LinkPtr( new Link() );
    }
    else if( objectType == "veModel" )
    {
        tmp = ModelPtr( new Model() );
    }
    else if( objectType == "veNetwork" )
    {
        tmp = NetworkPtr( new Network() );
    }
    else if( objectType == "Model" )
    {
        tmp = ModelPtr( new Model() );
    }
    else if( objectType == "Network" )
    {
        tmp = NetworkPtr( new Network() );
    }
    else if( objectType == "Point" )
    {
        tmp = PointPtr( new Point() );
    }
    else if( objectType == "Port" )
    {
        tmp = PortPtr( new Port() );
    }
    else if( objectType == "Tag" )
    {
        tmp = TagPtr( new Tag() );
    }
    else if( objectType == "System" )
    {
        tmp = SystemPtr( new System() );
    }
    else if( objectType == "veSystem" )
    {
        tmp = SystemPtr( new System() );
    }
    return tmp;
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr
ModelCreator::CreateNewXMLObjectCopy( const std::string& objectType,
                                      const ves::open::xml::XMLObjectPtr& objectToCopy )
{
    ves::open::xml::XMLObjectPtr tmp = ves::open::xml::XMLObjectPtr();
    if( objectType == "Link" )
    {
        tmp = LinkPtr( new Link(
            *( boost::dynamic_pointer_cast<Link>( objectToCopy ) ) ) );
    }
    else if( objectType == "Model" )
    {
        tmp = ModelPtr( new Model(
            *( boost::dynamic_pointer_cast<Model>( objectToCopy ) ) ) );
    }
    else if( objectType == "Network" )
    {
        tmp = NetworkPtr( new Network(
            *( boost::dynamic_pointer_cast<Network>( objectToCopy ) ) ) );
    }
    else if( objectType == "veModel" )
    {
        tmp = ModelPtr( new Model(
            *( boost::dynamic_pointer_cast<Model>( objectToCopy ) ) ) );
    }
    else if( objectType == "veNetwork" )
    {
        tmp = NetworkPtr( new Network(
            *( boost::dynamic_pointer_cast<Network>( objectToCopy ) ) ) );
    }
    else if( objectType == "Point" )
    {
        tmp = PointPtr( new Point(
            *( boost::dynamic_pointer_cast<Point>( objectToCopy ) ) ) );
    }
    else if( objectType == "Port" )
    {
        tmp = PortPtr( new Port(
            *( boost::dynamic_pointer_cast<Port>( objectToCopy ) ) ) );
    }
    else if( objectType == "Tag" )
    {
        tmp = TagPtr( new Tag(
            *( boost::dynamic_pointer_cast<Tag>( objectToCopy ) ) ) );
    }
    else if( objectType == "System" )
    {
        tmp = SystemPtr( new System(
            *( boost::dynamic_pointer_cast<System>( objectToCopy ) ) ) );
    }
    else if( objectType == "veSystem" )
    {
        tmp = SystemPtr( new System(
            *( boost::dynamic_pointer_cast<System>( objectToCopy ) ) ) );
    }
    return tmp;
}
////////////////////////////////////////////////////////////////////////////////
