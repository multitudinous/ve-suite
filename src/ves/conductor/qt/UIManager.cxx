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
//#define VES_QT_RENDER_DEBUG

#include <iostream>

// --- OpenSceneGraph includes --- //
#include <osg/Geometry>
#include <osg/ShadeModel>
#include <osg/Group>
#include <osg/Camera>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/StateAttribute>
#include <osg/Texture2D>
#include <osg/Projection>
#include <osg/NodeCallback>
#include <osg/Material>

#include <ves/conductor/qt/UIManager.h>
#include <ves/conductor/qt/UIElement.h>
#include <ves/conductor/qt/UIUpdateCallback.h>

#include <ves/xplorer/eventmanager/InteractionEvent.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>

using namespace ves::conductor;
using namespace ves;

vprSingletonImp( UIManager );
////////////////////////////////////////////////////////////////////////////////

UIManager::UIManager( )
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
    typedef boost::signals2::signal< void (xplorer::eventmanager::InteractionEvent&) > InteractionSignal_type;
    InteractionSignal_type::slot_type
    slotFunctor( boost::bind( &UIManager::SendInteractionEvent, this, _1 ) );
    xplorer::eventmanager::SlotWrapper< InteractionSignal_type > slotWrapper( slotFunctor );
    xplorer::eventmanager::EventManager::instance()->ConnectSignal( "KeyboardMouseInteractionSignal", &slotWrapper, mConnections, xplorer::eventmanager::EventManager::highest_Priority );
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
    osg::ref_ptr<osg::Switch> switch_node = new osg::Switch( );
    osg::ref_ptr<osg::MatrixTransform> transform = new osg::MatrixTransform( );
    osg::ref_ptr<osg::Geode> geode = new osg::Geode( );

    switch_node->addChild( transform.get( ) );
    transform->addChild( geode.get( ) );

    // Store the switch node so that it can be added to mUIGroup during the
    // next update traversal.
    mNodesToAdd.push_back( switch_node.get( ) );

    // Create a quad and a texture stateset for this UIElement
    // ********************* Begin Modified code taken from Paul Martz's book **
    // Create an object to store geometry in.
    osg::ref_ptr<osg::Geometry> geometry = new osg::Geometry;
    // Create an array of four vertices.
    osg::ref_ptr<osg::Vec3Array> vertices = new osg::Vec3Array;
    geometry->setVertexArray( vertices.get( ) );
    float m_width = element->GetElementWidth( );
    float m_height = element->GetElementHeight( );
    // 4------3
    // |      |
    // 1------2
    // We are in the x-y plane, NOT the x-z plane, as we should be. This is
    // because OSG treats ortho projection matrices as x-y plane by default.
    // Which, honestly, is a bit confounding, since ortho osg::camera views
    // are in the x-z plane by default. Grumble, grumble.
    vertices->push_back( osg::Vec3( 0.f, 0.f, 0.f ) ); // 1
    vertices->push_back( osg::Vec3( m_width, 0.f, 0.f ) ); // 2
    vertices->push_back( osg::Vec3( m_width, m_height, 0.f ) ); // 3
    vertices->push_back( osg::Vec3( 0.f, m_height, 0.f ) ); // 4

    // Create an array for the single normal.
    osg::ref_ptr<osg::Vec3Array> normal = new osg::Vec3Array;
    geometry->setNormalArray( normal.get( ) );
    geometry->setNormalBinding( osg::Geometry::BIND_OVERALL );

    // FIXME: Why must the normal be set > 1, even in ortho mode with no xform above?
    // Normal if in x-y plane
#ifdef VES_QT_RENDER_DEBUG
    normal->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
#else
    normal->push_back( osg::Vec3( 0.0f, 0.0f, 2.0f ) );
#endif

    // Draw a four-vertex quad from the stored data.
    geometry->addPrimitiveSet(
                               new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );

    geode->addDrawable( geometry.get( ) );

    // Create a Vec2Array of texture coordinates for texture unit 0
    //   and attach it to the geometry.
    osg::ref_ptr<osg::Vec2Array> texture_coordinates = new osg::Vec2Array;
    geometry->setTexCoordArray( 0, texture_coordinates.get( ) );
    osg::Vec4f coordinates = element->GetTextureCoordinates( );
    float m_left = coordinates.w( );
    float m_right = coordinates.x( );
    float m_bottom = coordinates.y( );
    float m_top = coordinates.z( );
    texture_coordinates->push_back( osg::Vec2( m_left, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_top ) );
    texture_coordinates->push_back( osg::Vec2( m_left, m_top ) );

    // Create an empty image
    osg::ref_ptr<osg::Image> image = new osg::Image( );

    // Attach the image in a Texture2D object
    osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D;
    // Don't rescale texture to power of two if hardware supports non-power of two
    texture->setResizeNonPowerOfTwoHint( false );
    texture->setImage( image.get( ) );
    texture->setDataVariance( osg::Object::DYNAMIC );

    // Create stateset for adding texture
    osg::StateSet* m_State = geode->getOrCreateStateSet( );

    // Attach the 2D texture attribute and enable GL_TEXTURE_2D,
    //   both on texture unit 0.
    m_State->setTextureAttributeAndModes( 0, texture );
    // ********************** End Modified code taken from Paul Martz's book **

    // Make sure that alpha blending is on for the UI elements
    m_State->setMode( GL_BLEND, osg::StateAttribute::ON );
    m_State->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

    // Attach a material that will allow changes to the element's opacity
    //    osg::Material *m_material = new osg::Material;
    //    m_material->setAlpha(osg::Material::FRONT_AND_BACK, 1.0f);
    //    m_State->setAttributeAndModes(m_material, osg::StateAttribute::ON );

    //m_State->setMode( GL_NORMALIZE, osg::StateAttribute::ON );
    //m_State->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    // Make sure osg doesn't optimize the UI element into oblivion
    geode->setDataVariance( osg::Object::DYNAMIC );

    mElements[ geode.get( ) ] = element;
    return geode.get( );
}
////////////////////////////////////////////////////////////////////////////////

bool UIManager::RemoveElement( osg::ref_ptr<osg::Geode> geode )
{
    // Search through to find geode, then delete UIElement and geode, then erase
    // entry from map
    return true;
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::RemoveAllElements( )
{
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
            ++map_iterator )
    {
        delete map_iterator->second;
        //map_iterator->first.release();
    }

    mElements.clear( );
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::Update( )
{
    // Ensure that initialization has already happened
    if( !mInitialized )
    {
        //Initialize();
        return;
    }

    // Insert any new elements added since last update into the scenegraph
    if( mNodesToAdd.size( ) > 0 )
    {
        _insertNodesToAdd( );
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
        if( mUIGroup->asSwitch( )->getValue( 0 ) )
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
        _showAll( );
    }
    else if( mHide )
    {
        _hideAll( );
    }

    // Check visibility of UI branch before bothering with repaints
    if( mUIGroup->getValue( 0 ) )
    {
        _repaintChildren( );
    }
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::HideAllElements( )
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

void UIManager::ToggleVisibility( )
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

    mProjection = new osg::Projection;
    //mProjection = new osg::MatrixTransform;
    mProjection->setMatrix( osg::Matrix::ortho2D( mLeft, mRight,
                                                  mBottom, mTop ) );

    osg::MatrixTransform* modelViewMatrix = new osg::MatrixTransform;
    modelViewMatrix->setMatrix( osg::Matrix::identity( ) );
    modelViewMatrix->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

    mProjection->addChild( modelViewMatrix );

    osg::StateSet* projectionStateSet = new osg::StateSet( );
    mProjection->setStateSet( projectionStateSet );
    projectionStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    projectionStateSet->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF );
    projectionStateSet->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );
    // Need to make sure this geometry is drawn last. RenderBins are handled
    // in numerical order so set bin number to 11
    // FIXME: Why RenderBin "11"? Does OSG only support 12 renderbins?
    projectionStateSet->setRenderBinDetails( 11, "RenderBin" );

    mUIGroup = new osg::Switch( );
    modelViewMatrix->addChild( mUIGroup.get( ) );

    parentNode->addChild( mProjection.get( ) );
    //parentNode->addChild( mUIGroup.get( ) );

    // Attach a material to the UIGroup that allows us to affect the opacity of
    // all UI elements at once
    osg::StateSet* m_UIGroupStateSet = mUIGroup->getOrCreateStateSet( );
    mOverallOpacity = new osg::Material;
#ifdef VES_QT_RENDER_DEBUG
    mOverallOpacity->setAlpha( osg::Material::FRONT_AND_BACK, 0.85f );
#else
    mOverallOpacity->setAlpha( osg::Material::FRONT_AND_BACK, 1.0f );
#endif
    m_UIGroupStateSet->setAttributeAndModes( mOverallOpacity.get( ), osg::StateAttribute::ON );

    mUIGroup->setDataVariance( osg::Object::DYNAMIC );
    mUIGroup->setUpdateCallback( mUIUpdateCallback.get() );

    mInitialized = true;
}
////////////////////////////////////////////////////////////////////////////////

//void UIManager::operator( )( osg::Node* node, osg::NodeVisitor* nv )
//{
//    // Request element repaints and so forth
//    Update( );
//
//    // Allow update traversal to continue
//    traverse( node, nv );
//}
////////////////////////////////////////////////////////////////////////////////

void UIManager::SendInteractionEvent( ves::xplorer::eventmanager::InteractionEvent &event )
{
    // Ignore events if we're not initialized
    if ( !mInitialized )
    {
        return;
    }

    // Check visibility of UI branch before bothering with events
    if( !mUIGroup->getValue( 0 ) )
    {
        return;
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
    for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
            ++map_iterator )
    {
        // Check whether this element is currently switched as visible. If not,
        // should not send events to it.
        if( map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch( )
                ->getValue( 0 ) )
        {
            UIElement* element = ( *map_iterator ).second;
            // Flip y mouse coordinate to origin GUI expects
            event.Y = static_cast < double > ( mTop ) - event.Y;
            element->SendInteractionEvent( event );
        }
    }

}
////////////////////////////////////////////////////////////////////////////////

void UIManager::_insertNodesToAdd( )
{
    std::vector< osg::ref_ptr<osg::Switch> >::iterator vec_iterator;
    for ( vec_iterator = mNodesToAdd.begin( );
            vec_iterator != mNodesToAdd.end( );
            ++vec_iterator )
    {
        mUIGroup->addChild( ( *vec_iterator ).get( ) );
        ( *vec_iterator ).release( );
    }

    mNodesToAdd.clear( );
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::_repaintChildren( )
{
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
            ++map_iterator )
    {
        // Check whether this element is currently switched as visible. If not,
        // no need to waste time rendering it.
        // FIXME: should only render *active*, visible element
        if( map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch( )
                ->getValue( 0 ) )
        {
            UIElement* element = ( *map_iterator ).second;
            unsigned char* image_Data = element->RenderElementToImage( );

            // Only reset the image if element tells us it has changed since 
            // last time
            if( element->IsDirty( ) )
            {
                osg::StateSet* state = ( *map_iterator ).first
                        ->getOrCreateStateSet( );
                osg::Image* image =
                        state->getTextureAttribute( 0, osg::StateAttribute::TEXTURE )
                        ->asTexture( )->getImage( 0 );
                image->setImage( element->GetImageWidth( ),
                                 element->GetImageHeight( ), 1, 4,
                                 GL_BGRA, GL_UNSIGNED_BYTE,
                                 image_Data, osg::Image::NO_DELETE );
                image->dirty( );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::_sendEvent( )
{

}
////////////////////////////////////////////////////////////////////////////////

void UIManager::_hideAll( )
{
    mUIGroup->setAllChildrenOff( );
    mHide = false;
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::_showAll( )
{
    //if( !showOnlyActive )
    {
        std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
        for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
                ++map_iterator )
        {
            map_iterator->first->getParent( 0 )->getParent( 0 )->asSwitch( )
                    ->setAllChildrenOn( );
        }
    }

    mUIGroup->setAllChildrenOn( );
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

void UIManager::UnembedAll( )
{
    if( !mInitialized )
    {
        return;
    }

    HideAllElements( );
    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
            ++map_iterator )
    {
        UIElement* element = ( *map_iterator ).second;
        element->Unembed( );
    }
}
////////////////////////////////////////////////////////////////////////////////

void UIManager::EmbedAll( )
{
    if( !mInitialized )
    {
        return;
    }

    std::map< osg::ref_ptr< osg::Geode >, UIElement* >::iterator map_iterator;
    for ( map_iterator = mElements.begin( ); map_iterator != mElements.end( );
            ++map_iterator )
    {
        UIElement* element = ( *map_iterator ).second;
        element->Embed( );
    }
    ShowAllElements( );
}
////////////////////////////////////////////////////////////////////////////////
