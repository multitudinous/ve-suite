/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
////////////////////////////////////////////////////////////////////////////////
#pragma once

#include <ves/VEConfig.h>

#include <ves/xplorer/Logging.h>

#include <vector>
#include <utility>

#include <osg/Matrixf>
#include <osg/Switch>
#include <osg/Geode>
#include <osg/AnimationPath>
#include <osg/Vec2d>
#include <osg/Image>

#include <gadget/Type/KeyboardMouse/Keys.h>

#include <switchwire/ScopedConnectionList.h>
#include <switchwire/InteractionEvent.h>

namespace osg
{
class Vec4f;
class MatrixTransform;
class AnimationPath;
}

namespace ves
{
namespace xplorer
{
namespace eventmanager
{
class InteractionEvent;
} // namespace eventmanager
} // namespace xplorer
namespace conductor
{
/// @file UIElement.h

////////////////////////////////////////////////////////////////////////////////
/// @class ves::conductor::UIElement
/// Abstract class that defines interaction with GL-embedded user interfaces
/// Derived classes must call the PostConstructor method at the very end
/// of their constructor to ensure that the scenegraph for the element is
/// set up properly.
////////////////////////////////////////////////////////////////////////////////
class VE_CONDUCTOR_QTUI_EXPORTS UIElement
{
public:
    ///Constructor
    UIElement();
    ///Destructor
    virtual ~UIElement();

    ///Set the initial image height and width to be used with this UIElement
    ///\param width The width of the image
    ///\param height The height of the image
    void SetInitialImageWidthAndHeight( int width, int height );

    ////////////////////////////////////////////////////////////////////////////////
    /// Returns the pixel width of the element's rendered image.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual int GetImageWidth();
    ////////////////////////////////////////////////////////////////////////////////
    /// Returns the pixel height of the element's rendered image.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual int GetImageHeight();
    ////////////////////////////////////////////////////////////////////////////////
    /// Returns the pixel width of the element itself.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual int GetElementWidth();
    ////////////////////////////////////////////////////////////////////////////////
    /// Returns the pixel height of the element itself.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual int GetElementHeight();
    ////////////////////////////////////////////////////////////////////////////////
    /// Returns the texture coordinates of the element inside its texture image.
    /// Four components of return value are w = left, x = right, y = bottom,
    /// z = top.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual const osg::Vec4f GetTextureCoordinates();
    ////////////////////////////////////////////////////////////////////////////////
    /// Used to send generic interaction events (mouse, keyboard, wand, etc.) to
    /// this element.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual void SendInteractionEvent( switchwire::InteractionEvent& event );

    virtual void SendButtonPressEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendButtonReleaseEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendDoubleClickEvent( gadget::Keys button, int x, int y, int state );
    virtual void SendMouseMoveEvent( int x, int y, int z, int state );
    virtual void SendKeyPressEvent( gadget::Keys key, int modifierMask, char unicode );
    virtual void SendKeyReleaseEvent( gadget::Keys key, int modifierMask, char unicode );
    virtual void SendScrollEvent( int deltaX, int deltaY, int x, int y, int state );
    ////////////////////////////////////////////////////////////////////////////////
    /// Tell this element to render to an image and return a pointer to the data.
    ///
    /// Child classes should use calls to this function as an opportunity to do two
    /// related things: (1) Update any UI parts that require such, and (2) Render
    /// the UI element to an image.
    /// @return Pointer to the image data.
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    //virtual unsigned char* RenderElementToImage();
    virtual osg::Image* RenderElementToImage();
    ////////////////////////////////////////////////////////////////////////////////
    // Should return true if rendered image changed in the most recent call to
    // RenderElementToImage; otherwise false. This is intended as a way to allow
    // callers to determine whether they can use a stored copy of the previously-
    // rendered image or must update to the new one pointed to by
    // RenderElementToImage
    ////////////////////////////////////////////////////////////////////////////////
    virtual bool IsDirty();
    ////////////////////////////////////////////////////////////////////////////////
    /// Give the element a chance to do any needed initialization before we show it
    /// and begin interacting with it. Elements might use this as an opportunity to
    /// start timers, set up events, or perform other initialization tasks that are
    /// not appropriate to doing inside a constructor.
    ///
    /// Required override.
    ////////////////////////////////////////////////////////////////////////////////
    virtual void Initialize();
    ////////////////////////////////////////////////////////////////////////////////
    // Should return true if image *size* has changed since last call to
    // RenderElementToImage; otherwise false.
    ////////////////////////////////////////////////////////////////////////////////
    virtual bool SizeDirty();

    virtual std::vector< std::pair< osg::ref_ptr<osg::Image>, std::pair< int, int > > > const& GetDamagedAreas();

    ///
    virtual void SetMinimized( bool state );

    ///
    virtual bool IsMinimized();

    ///
    virtual osg::Matrixf& GetUIMatrix();

    ///
    virtual void PushUIMatrix( osg::Matrixf const& matrix );

    ///
    virtual osg::Matrixf PopUIMatrix();

    ///
    //virtual void PushElementMatrix( osg::Matrixf const& matrix );

    ///
    //virtual osg::Matrixf& GetElementMatrix();

    ///
    virtual void MoveCanvas( float dx, float dy, float dz = 0.0f );

    /// Resize this element to width x height
    virtual void ResizeCanvas( int width, int height );

    ///
    virtual void Update();

    ///
    //virtual osg::MatrixTransform* GetUITransform();

    ///
    //virtual osg::MatrixTransform* GetElementTransform();

    ///
    virtual bool IsVisible();

    ///
    virtual void SetVisible( bool visible );

    ///
    virtual void SetAnimationPath( osg::AnimationPath* path );

    ///
    //virtual osg::Switch* GetVisibilitySwitch();

    ///
    virtual osg::Geode* GetGeode();

    /// Tell this element it is on top. Subclasses may choose to render
    /// differently when on top.
    virtual void Raise()
    {
        ;
    }

    /// Tell this element it is no longer on top. Subclasses may choose to render
    /// differently when not on top.
    virtual void Lower()
    {
        ;
    }

    /// Set whether titlebar is displayed
    virtual void ShowTitlebar( bool )
    {
        ;
    }

    /// Toggle whether titlebar is displayed
    virtual void ToggleTitlebar() {}

    /// This is called prior to deleting the element to allow for any special
    /// cleanup to be done that may be sensitive to thread context.
    virtual void Cleanup() {}

    ///Get the intersection point in pixels from a mouse press or wand test
    void GetPointIntersectionInPixels( int& x, int& y, osg::Vec3d& point );

    ///Update the min and max corners for the UI element
    ///\note This method sets the size for the UI element in desktop control mode
    void ComputeMouseBoundsForElement();

    ///Test the quad for intersections with mouse coordinates
    bool TestQuadIntersection( int x, int y );

    ///Get the corners for this quad
    osg::Vec4d& GetUICorners();

    ///Convert pixel coords to texture coords
    osg::Vec2d& GetTextureCoords( int x, int y );

    ///Is the element initialized
    bool IsInitialized() const;

protected:
    ///Tell whether the mouse is over the UI
    void UIEnterLeave( bool uiEnter );

    /// Sets up the sub-branch for the scenegraph. Derived classes must call this
    /// at the very end of their constructor.
    void PostConstructor();

    /// Flag telling whether this element has been initialized
    bool mInitialized;
    ///Is the UI minimized
    bool mIsMinimized;
    bool mUIMatrixDirty;
    std::vector< osg::Matrixf > mUIMatrices;
    osg::ref_ptr< osg::Vec3Array > m_vertices;
    bool mAnimationOn;
    osg::ref_ptr< osg::AnimationPath > m_animationPath;
    ///Container for the UI image data
    osg::ref_ptr< osg::Geode > mGeode;

    ///The resolution of the initial image size
    std::pair< int, int > m_initialImageSize;
    ///Tell whether the UI enter or leave
    bool m_mouseInsideUI;
    /// Required to be able to connect up to signals.
    switchwire::ScopedConnectionList m_connections;
    ///Keep a ratio of the number of pixels per foot for non-desktop mode
    double m_pixelUIRatio;
    ///The min and max corners for this element
    osg::Vec4d m_uiCorners;
    ///Texture coord
    osg::Vec2d m_texCoords;
    ///UI size for cave mode
    std::pair< int, int > m_uiSize;
    ///The UI texture
    osg::ref_ptr< osg::Image > m_osgImage;
    ///Track damaged areas of the UI
    std::vector< std::pair< osg::ref_ptr<osg::Image>, std::pair< int, int > > > m_damagedAreas;
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
};

} // namepsace conductor
} // namespace ves
