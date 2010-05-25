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
////////////////////////////////////////////////////////////////////////////////
#ifndef UIELEMENT_H
#define UIELEMENT_H

#include <osg/Vec4f>

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
////////////////////////////////////////////////////////////////////////////////
class UIElement
{
public:
////////////////////////////////////////////////////////////////////////////////
/// Returns the pixel width of the element's rendered image.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual int GetImageWidth() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Returns the pixel height of the element's rendered image.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual int GetImageHeight() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Returns the pixel width of the element itself.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual int GetElementWidth() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Returns the pixel height of the element itself.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual int GetElementHeight() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Returns the texture coordinates of the element inside its texture image.
/// Four components of return value are w = left, x = right, y = bottom,
/// z = top.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual const osg::Vec4f GetTextureCoordinates() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Used to send generic interaction events (mouse, keyboard, wand, etc.) to
/// this element.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual void SendInteractionEvent( xplorer::eventmanager::InteractionEvent &event ) = 0;
////////////////////////////////////////////////////////////////////////////////
/// Tell this element to render to an image and return a pointer to the data.
///
/// Child classes should use calls to this function as an opportunity to do two
/// related things: (1) Update any UI parts that require such, and (2) Render
/// the UI element to an image.
/// @return Pointer to the image data.
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual unsigned char* RenderElementToImage() = 0;
////////////////////////////////////////////////////////////////////////////////
// Should return true rendered image changed in the most recent call to 
// RenderElementToImage; otherwise false. This is intended as a way to allow
// callers to determine whether they can use a stored copy of the previously-
// rendered image or must update to the new one pointed to by 
// RenderElementToImage
////////////////////////////////////////////////////////////////////////////////
    virtual bool IsDirty() = 0;
////////////////////////////////////////////////////////////////////////////////
/// Give the element a chance to do any needed initialization before we show it
/// and begin interacting with it. Elements might use this as an opportunity to
/// start timers, set up events, or perform other initialization tasks that are
/// not appropriate to doing inside a constructor.
///
/// Required override.
////////////////////////////////////////////////////////////////////////////////
    virtual void Initialize(  ) = 0;
////////////////////////////////////////////////////////////////////////////////
    virtual void Unembed(  ) = 0;
    virtual void Embed(  ) = 0;

};

} // namepsace conductor
} // namespace ves

#endif // UIELEMENT_H
