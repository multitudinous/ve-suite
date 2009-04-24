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
// --- VE-Suite Includes --- //
#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/scenegraph/TextTexture.h>

#include <osgBullet/Chart.h>

// --- OSG Includes --- //
#include <osg/Geode>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

using namespace ves::xplorer::environment;

////////////////////////////////////////////////////////////////////////////////
TextTextureCallback::TextTextureCallback( vxs::TextTexture* textTexture )
    :
    m_textTexture( textTexture )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TextTextureCallback::~TextTextureCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TextTextureCallback::TextTextureCallback( const TextTextureCallback& ctc, const osg::CopyOp& copyop )
    :
    osg::NodeCallback( ctc, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TextTextureCallback::operator()(
    osg::Node* node, osg::NodeVisitor* nv )
{
    //osg::ref_ptr< osg::TextTexture > t =
    //    static_cast< osg::TextTexture* >( node );
    m_textTexture->GetChart()->setValue( nv->getTraversalNumber(), nv->getTraversalNumber()%20 );
    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
