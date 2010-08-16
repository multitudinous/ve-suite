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

// --- VES Includes --- //
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/AutoTransform.h>

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/Geode>

#include <osgUtil/IntersectionVisitor>

#include <osgText/Text>
#include <osgText/Font>

#include <osgwTools/AbsoluteModelTransform.h>
#include <osgwTools/Shapes.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
osgUtil::LineSegmentIntersector::Intersections& TestForIntersections(
    osgUtil::LineSegmentIntersector& intersector,
    osg::Node& root,
    unsigned int traversalMask )
{
    osgUtil::IntersectionVisitor intersectionVisitor( &intersector );
    intersectionVisitor.setTraversalMask( traversalMask );
    root.accept( intersectionVisitor );

    return intersector.getIntersections();
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* FindVESObject( osg::NodePath& nodePath )
{
    osg::NodePath::reverse_iterator itr = nodePath.rbegin();
    for( itr; itr != nodePath.rend(); ++itr )
    {
        osg::Node::DescriptionList descList = (*itr)->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            if( descList.at( i ) == "VE_XML_ID" )
            {
                return *itr;
            }
        }
        //Trim the node path
        nodePath.pop_back();
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* CreateCircleHighlight( const osg::Vec3 eyePoint, 
    const osg::NodePath& nodePath, const osg::Node& pickedNode, 
    const std::string& labelText )
{
    const std::string textAnnotation( labelText );
    
    // determine Subdivision and Radius settings for current viewpoint (stub for now)
    osg::BoundingSphere sphere( pickedNode.getBound() );
    const double radius( sphere.radius() );
    //osg::Vec3 dVec( sphere.center() - eyePoint );
    //const double distance( dVec.length() );
    
    // Subdivision segments is inversely proportional to distance/radius.
    // If distance/radiue if halved, segments is doubled, and vice versa.
    // Basis: subdivide circle with 60 segments at a distance/radius of 10 units.
    const int subdivisions( 30 );//(int)( 10.f / ( distance / radius ) * 60.f ) );
    osg::notify( osg::DEBUG_FP ) << "  Using subdiv " << subdivisions << std::endl;
    
    // Determine text pos and line segment endpoints.
    osg::Vec3 textDirection( 1., 1., 0. );
    textDirection.normalize();
    osg::Vec3 lineEnd( textDirection * radius );
    osg::Vec3 textPos( textDirection * radius * 1.4f );
    
    // Structure:
    //   AbsoluteModelTransform->AutoTransform->CircleGeode->Circle (Geometry)
    //                                                   \-->Line segment (Geometry)
    //                                                   \-->Label (osgText::Text)
    osg::ref_ptr< osg::Geode > circlegeode;
    osg::ref_ptr< AutoTransform > circleat;
    osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt;
    
    // determine position of highlight
    osg::Vec3 position(0., 0., 0.);
    osg::Matrix matrix = osg::computeLocalToWorld(nodePath);
    position = nodePath[nodePath.size()-1]->getBound().center();
    
    circlegeode = new osg::Geode();
    osg::Geometry* circleGeom( osgwTools::makeWireCircle( radius, subdivisions ) );
    circlegeode->addDrawable( circleGeom );
    circleat = new AutoTransform();
    circleat->addChild( circlegeode.get() );
    circleat->SetAutoRotateMode( 
        ves::xplorer::scenegraph::AutoTransform::ROTATE_TO_CAMERA );
    circleat->SetAutoScaleToScreen(false);
    circleat->SetPosition(position);
    amt = new osgwTools::AbsoluteModelTransform();
    amt->addChild( circleat.get() );
    // setup Absolute Model Transform to mimic transforms of nodepath
    amt->setMatrix(matrix); 
    
    // Add a line segment from the circle to the text.
    {
        osg::Geometry* lineGeom = new osg::Geometry;
        circlegeode->addDrawable( lineGeom );
        
        osg::Vec3Array* verts( new osg::Vec3Array );
        verts->resize( 2 );
        (*verts)[ 0 ] = lineEnd;
        (*verts)[ 1 ] = textPos;
        lineGeom->setVertexArray( verts );
        
        lineGeom->setColorArray( circleGeom->getColorArray() );
        lineGeom->setColorBinding( osg::Geometry::BIND_OVERALL );
        
        lineGeom->addPrimitiveSet( new osg::DrawArrays( GL_LINES, 0, 2 ) );
    }
    
    if(!textAnnotation.empty())
    {
        // Add text annotation
        osg::ref_ptr<osgText::Text> text = new osgText::Text;
        text->setPosition( textPos );
        text->setFont( "arial.ttf" );
        text->setText( textAnnotation );
        text->setColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        text->setAlignment( osgText::Text::LEFT_BOTTOM );
        text->setAxisAlignment( osgText::Text::XY_PLANE );
        
        // Character size goes up as a function of distance.
        // Basis: Size is 0.1 for a distance of 10.0.
        //float size( 0.01f * distance );
        float size( 1.0f );
        osg::notify( osg::DEBUG_FP ) << "    Using char size " 
            << size << std::endl;
        text->setCharacterSize( size );
        
        circlegeode->addDrawable( text.get() );
    } // if
    
    // TBD application responsibility?
    // turn off depth testing on our subgraph
    amt->getOrCreateStateSet()->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
    amt->getOrCreateStateSet()->setRenderBinDetails( 1000, "RenderBin" );
    amt->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
    
    return( amt.release() );
}
////////////////////////////////////////////////////////////////////////////////
} //end scenegraph
} //end xplorer
} //end ves
