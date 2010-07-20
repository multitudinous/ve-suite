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
#include <ves/conductor/qt/UIManager.h>
#include <ves/conductor/qt/UIElement.h>
#include <ves/conductor/qt/UIUpdateCallback.h>

#include <ves/xplorer/eventmanager/InteractionEvent.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Group>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/StateAttribute>
#include <osg/Texture2D>
#include <osg/Projection>
#include <osg/NodeCallback>
#include <osg/Material>
#include <osg/Depth>

// --- STL Includes --- //
#include <iostream>

//#define VES_QT_RENDER_DEBUG

using namespace ves::conductor;

vprSingletonImp( UIManager );

////////////////////////////////////////////////////////////////////////////////
UIManager::UIManager()
{
    mInitialized = false;
    mLeft = 0;
    mRight = 640;
    mBottom = 0;
    mTop = 480;
    mRectangleDirty = false;
    mToggleVisibility = false;
    mHide = false;
    mShow = false;
    mUIUpdateCallback = new UIUpdateCallback;

    typedef boost::signals2::signal< bool (xplorer::eventmanager::InteractionEvent&) > InteractionSignal_type;
    InteractionSignal_type::slot_type
    slotFunctor( boost::bind( &UIManager::SendInteractionEvent, this, _1 ) );
    xplorer::eventmanager::SlotWrapper< InteractionSignal_type > slotWrapper( slotFunctor );
    xplorer::eventmanager::EventManager::instance()->ConnectSignal( "KeyboardMouseInteractionSignal", &slotWrapper, mConnections, xplorer::eventmanager::EventManager::highest_Priority );

    typedef boost::signals2::signal< void () > HideShowUISignal_type;
    HideShowUISignal_type::slot_type hsSlotFunctor( boost::bind (&UIManager::ToggleVisibility, this) );
    xplorer::eventmanager::SlotWrapper< HideShowUISignal_type > hsSlotWrapper( hsSlotFunctor );
    xplorer::eventmanager::EventManager::instance()->ConnectSignal( "KeyboardMouse.HideShowUISignal", &hsSlotWrapper, mConnections, xplorer::eventmanager::EventManager::highest_Priority );
}
////////////////////////////////////////////////////////////////////////////////
UIManager::~UIManager()
{
    if( mUIGroup.valid() )
    {
        mUIGroup->removeUpdateCallback( mUIUpdateCallback.get() );
    }

    //std::cout << this->referenceCount() << std::endl;
    // Delete all UIElements of which we've taken charge
    // Note that these were not allocated inside this class, but the class
    // interface specifies that it takes ownership of these objects
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        //delete ( *map_iterator ).second;
    }
    mElements.clear();
    // All other memory allocated on the heap by this class should be attached
    // to an osg::ref_ptr and so should automatically manage its lifetime
    //std::cout << " UI manager destructor" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* UIManager::AddElement( UIElement* element )
{
    //
    float q_right = element->GetElementWidth();
    float q_top = element->GetElementHeight();
    float q_left = 0.0;
    float q_bottom = 0.0;
    osg::Vec4 ortho2DQuadPosition( q_left, q_right, q_bottom, q_top );
    mElementPositionsOrtho2D.push_back( ortho2DQuadPosition );

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;
    vertices->push_back( osg::Vec3( q_left, q_bottom, 0.0 ) );
    vertices->push_back( osg::Vec3( q_right, q_bottom, 0.0 ) );
    vertices->push_back( osg::Vec3( q_right, q_top, 0.0 ) );
    vertices->push_back( osg::Vec3( q_left, q_top, 0.0 ) );

    //
    osg::Vec4f coordinates = element->GetTextureCoordinates();
    float m_left = coordinates.w();
    float m_right = coordinates.x();
    float m_bottom = coordinates.y();
    float m_top = coordinates.z();

    osg::ref_ptr< osg::Vec2Array > texture_coordinates = new osg::Vec2Array();
    texture_coordinates->push_back( osg::Vec2( m_left, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_top ) );
    texture_coordinates->push_back( osg::Vec2( m_left, m_top ) );

    //
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    geometry->setVertexArray( vertices.get() );
    geometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    geometry->setTexCoordArray( 0, texture_coordinates.get() );

    //
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->setCullingActive( false );
    geode->setDataVariance( osg::Object::DYNAMIC );
    geode->addDrawable( geometry.get() );
    mElements[ geode.get() ] = element;

    //Create an empty image
    osg::ref_ptr< osg::Image > image = new osg::Image();

    //Attach the image in a Texture2D object
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
    texture->setResizeNonPowerOfTwoHint( false );
    texture->setImage( image.get() );
    texture->setDataVariance( osg::Object::DYNAMIC );

    //Vertex shader not used yet
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    std::string vertexSource =
    "void main() \n"
    "{ \n"
        //Ignore MVP transformation as vertices are already in Normalized Device Coord.
        "gl_Position = gl_Vertex; \n"
        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
    "} \n";

    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
    vertexShader->setName( "VS Quad Vertex Shader" );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
    "uniform sampler2D baseMap; \n"

    "uniform vec3 glowColor; \n"

    "void main() \n"
    "{ \n"
        "vec4 baseColor = texture2D( baseMap, gl_TexCoord[ 0 ].st ); \n"

        "gl_FragData[ 0 ] = baseColor; \n"
        "gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
    "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS Quad Fragment Shader" );

    //
    osg::ref_ptr< osg::Program > program = new osg::Program();
    //program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    program->setName( "VS Quad Program" );

    //Create stateset for adding texture
    osg::ref_ptr< osg::StateSet > stateset = geode->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
    stateset->setAttributeAndModes(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    stateset->setTextureAttributeAndModes(
        0, texture.get(), osg::StateAttribute::ON );

    //
    osg::ref_ptr< osg::MatrixTransform > transform = new osg::MatrixTransform();
    transform->addChild( geode.get() );

    //
    osg::ref_ptr< osg::Switch > switch_node = new osg::Switch();
    switch_node->addChild( transform.get() );
    //Store the switch node so that it can be added to mUIGroup during the
    //next update traversal.
    mNodesToAdd.push_back( switch_node.get() );

    return geode.get();
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::RemoveElement( osg::ref_ptr<osg::Geode> geode )
{
    // Search through to find geode, then delete UIElement and geode, then erase
    // entry from map
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::RemoveAllElements()
{
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        delete map_iterator->second;
        //map_iterator->first.release();
    }

    mElements.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::Update()
{
    // Ensure that initialization has already happened
    if( !mInitialized )
    {
        //Initialize();
        return;
    }

    // Insert any new elements added since last update into the scenegraph
    if( mNodesToAdd.size() > 0 )
    {
        _insertNodesToAdd();
    }

    // Update the UI's rectangle if it has been dirtied.
    if( mRectangleDirty )
    {
        mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
                                                      mBottom, mTop ) );
        //mProjection->setMatrix( mTempProj );
        mRectangleDirty = false;
    }

    // Do hide/show operations
    if( mToggleVisibility )
    {
        if( mUIGroup->asSwitch()->getValue( 0 ) )
        {
            mShow = false;
            mHide = true;
        }
        else
        {
            mShow = true;
            mHide = false;
        }

        mToggleVisibility = false;
    }

    if( mShow )
    {
        _showAll();
    }
    else if( mHide )
    {
        _hideAll();
    }

    // Check visibility of UI branch before bothering with repaints
    if( mUIGroup->getValue( 0 ) )
    {
        _repaintChildren();
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::HideAllElements()
{
    // May not be able to touch scenegraph directly at the moment;
    // Set hide flag to be discovered during update
    mHide = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ShowAllElements( bool showOnlyActive )
{
    // May not be able to touch scenegraph directly at the moment;
    // Set show flag to be discovered during update
    mShow = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::ToggleVisibility()
{
    // May not be able to touch scenegraph directly at the moment;
    // Set visibility flag to be discovered during update
    mToggleVisibility = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetRectangle( int left, int right, int bottom, int top )
{
    mRectangleDirty = true;
    mLeft = left;
    mRight = right;
    mBottom = bottom;
    mTop = top;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::Initialize( osg::Group* parentNode )
{
    //std::cout << "UIManager::Initialize" << std::endl;
    // Only allow initialization to happen once.
    if( mInitialized )
    {
        return;
    }

    mProjection = new osg::Projection();
    mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
                                                  mBottom, mTop ) );

    osg::MatrixTransform* modelViewMatrix = new osg::MatrixTransform;
    modelViewMatrix->setMatrix( osg::Matrix::identity() );
    modelViewMatrix->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    mProjection->addChild( modelViewMatrix );

    //Set depth test to always pass and don't write to the depth buffer
    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );

    //
    osg::ref_ptr< osg::StateSet > stateset = mProjection->getOrCreateStateSet();
    stateset->setRenderBinDetails( 99, "RenderBin" );
    stateset->setAttributeAndModes(
        depth.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    //stateset->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF );

    mUIGroup = new osg::Switch();
    modelViewMatrix->addChild( mUIGroup.get() );

    parentNode->addChild( mProjection.get() );

    // Attach a material to the UIGroup that allows us to affect the opacity of
    // all UI elements at once
    //osg::StateSet* m_UIGroupStateSet = mUIGroup->getOrCreateStateSet();
    //mOverallOpacity = new osg::Material;
#ifdef VES_QT_RENDER_DEBUG
    mOverallOpacity->setAlpha( osg::Material::FRONT_AND_BACK, 0.85f );
#else
    //mOverallOpacity->setAlpha( osg::Material::FRONT_AND_BACK, 1.0f );
#endif
    //m_UIGroupStateSet->setAttributeAndModes( mOverallOpacity.get(), osg::StateAttribute::ON );

    mUIGroup->setDataVariance( osg::Object::DYNAMIC );
    mUIGroup->setUpdateCallback( mUIUpdateCallback.get() );

    mInitialized = true;
}
////////////////////////////////////////////////////////////////////////////////
//void UIManager::operator()( osg::Node* node, osg::NodeVisitor* nv )
//{
//    // Request element repaints and so forth
//    Update();
//
//    // Allow update traversal to continue
//    traverse( node, nv );
//}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent &event )
{
    // Ignore events if we're not initialized
    if ( !mInitialized )
    {
        return false;
    }

    // Check visibility of UI branch before bothering with events
    if( !mUIGroup->getValue( 0 ) )
    {
        return false;
    }

    // If we're dealing with a mouse event, see if it's over one of our managed
    // quads. If it isn't, we ignore the event.
    if( (event.EventType == ves::xplorer::eventmanager::InteractionEvent::pointerMotion) ||
        (event.EventType == ves::xplorer::eventmanager::InteractionEvent::buttonPress) ||
        (event.EventType == ves::xplorer::eventmanager::InteractionEvent::buttonRelease) )
    {
        if( !Ortho2DTestPointerCoordinates( event.X, event.Y ) )
        {
            return false;
        }
    }

    // Currently we have no logic in place to determine *which* element should
    // receive this event. When there are multiple Geodes, the passed event
    // should include a pointer to the Geode that had mouse/wand intersection
    // (for mouse events). We should be storing the focused element somewhere
    // in this manager class for keyboard events. However, since the current
    // implementation uses only one Geode and allows the mdi area in that geode
    // to manage subwindows, we don't need the information about which element
    // to send things to: there is really only one element. Even so, we iterate
    // through "all" the elements below, just in case.

    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        // Check whether this element is currently switched as visible. If not,
        // should not send events to it.
        if( map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch()
                ->getValue( 0 ) )
        {
            UIElement* element = ( *map_iterator ).second;
            // Flip y mouse coordinate to origin GUI expects
            event.Y = static_cast < double > ( mTop ) - event.Y;
            element->SendInteractionEvent( event );
        }
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_insertNodesToAdd()
{
    std::vector< osg::ref_ptr<osg::Switch> >::iterator vec_iterator;
    for ( vec_iterator = mNodesToAdd.begin();
            vec_iterator != mNodesToAdd.end();
            ++vec_iterator )
    {
        mUIGroup->addChild( ( *vec_iterator ).get() );
        ( *vec_iterator ).release();
    }

    mNodesToAdd.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_repaintChildren()
{
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        // Check whether this element is currently switched as visible. If not,
        // no need to waste time rendering it.
        // FIXME: should only render *active*, visible element
        if( map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch()
                ->getValue( 0 ) )
        {
            UIElement* element = ( *map_iterator ).second;
            unsigned char* image_Data = element->RenderElementToImage();

            // Only reset the image if element tells us it has changed since 
            // last time
            if( element->IsDirty() )
            {
                osg::StateSet* state = ( *map_iterator ).first
                        ->getOrCreateStateSet();
                osg::Image* image =
                        state->getTextureAttribute( 0, osg::StateAttribute::TEXTURE )
                        ->asTexture()->getImage( 0 );
                image->setImage( element->GetImageWidth(),
                                 element->GetImageHeight(), 1, 4,
                                 GL_BGRA, GL_UNSIGNED_BYTE,
                                 image_Data, osg::Image::NO_DELETE );
                image->dirty();
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_sendEvent()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_hideAll()
{
    mUIGroup->setAllChildrenOff();
    mHide = false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::_showAll()
{
    //if( !showOnlyActive )
    {
        std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
        for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
                ++map_iterator )
        {
            map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch()
                    ->setAllChildrenOn();
        }
    }

    mUIGroup->setAllChildrenOn();
    mShow = false;
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::SetProjectionMatrix( osg::Matrixd& matrix )
{
    if( matrix != mTempProj )
    {
        mRectangleDirty = true;
        mTempProj = matrix;
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::UnembedAll()
{
    if( !mInitialized )
    {
        return;
    }

    HideAllElements();
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        UIElement* element = ( *map_iterator ).second;
        element->Unembed();
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIManager::EmbedAll()
{
    if( !mInitialized )
    {
        return;
    }

    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin(); map_iterator != mElements.end();
            ++map_iterator )
    {
        UIElement* element = ( *map_iterator ).second;
        element->Embed();
    }
    ShowAllElements();
}
////////////////////////////////////////////////////////////////////////////////
bool UIManager::Ortho2DTestPointerCoordinates( int x, int y )
{
    // Walk through every quad we own and see if the point lies on it
    osg::Vec4 quadPos;
    for( size_t index = 0; index < mElementPositionsOrtho2D.size(); index++)
    {
        quadPos = mElementPositionsOrtho2D.at( index );
        if( ( x >= quadPos.x()  ) && ( x <= quadPos.y() ) &&
                ( y >= quadPos.z() ) && ( y <= quadPos.w() ) )
        {
            return true;
        }
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
