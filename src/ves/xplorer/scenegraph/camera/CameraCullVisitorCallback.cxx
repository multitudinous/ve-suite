/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/camera/CameraCullVisitorCallback.h>

// --- OSG Includes --- //
#include <osg/Image>
#include <osg/io_utils>
#include <osg/Texture2D>
#include <osgUtil/CullVisitor>

#include <osgDB/WriteFile>
#include <osgDB/FileUtils>

#include <iostream>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraCullVisitorCallback::CameraCullVisitorCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraCullVisitorCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::Camera* tempCamera = static_cast< osg::Camera* >( node );
    std::cout << tempCamera << std::endl;
    if( nv->getVisitorType() == osg::NodeVisitor::CULL_VISITOR  )
    {
        osgUtil::CullVisitor* tempVisitor = static_cast< osgUtil::CullVisitor* >( nv );
        tempVisitor->getRenderStage()->setCameraRequiresSetUp( true );
    }
    traverse(node,nv);
}
////////////////////////////////////////////////////////////////////////////////
