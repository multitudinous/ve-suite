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

#ifndef VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H
#define VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H

// --- VES Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/Group>

// --- STL Includes --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace highlight
{
class CircleHighlight;

/*!\file HighlightManager.h
 * HighlightManager API
 */

/*!\class ves::xplorer::scenegraph::highligh::HighlightManager
 *
 */
class VE_SCENEGRAPH_EXPORTS HighlightManager : public osg::Group
{
public:
    ///Constructor
    HighlightManager();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    HighlightManager(
        const HighlightManager& highlightManager,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::highlight, HighlightManager );

    ///Override the addChild function to only accept Cameras
    virtual bool addChild( std::string const& name );

    ///
    CircleHighlight* const ConvertNodeToCircleHighlight( osg::Node* const node );

    ///
    void Enable( const bool& enable = true );

    ///
    CircleHighlight* const GetActiveCircleHighlight() const;

    ///Override the insertChild function to only accept Cameras
    virtual bool insertChild( unsigned int index, CircleHighlight* child );

    ///
    const bool IsEnabled() const;

    ///
    virtual void removeChildren();

    ///Override the replaceChild function to only accept Cameras
    virtual bool replaceChild(
        CircleHighlight* origChild, CircleHighlight* newChild );

    ///
    void SetActiveCircleHighlight( CircleHighlight* circleHighlight );

    ///Override the setChild function to only accept Cameras
    virtual bool setChild( unsigned int i, CircleHighlight* node );

protected:
    ///Destructor
    virtual ~HighlightManager();

private:
    ///Is the highlight manager turned on
    bool m_enabled;

    ///The active highlight
    CircleHighlight* m_activeCircleHighlight;

};
} //end highlight
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_HIGHLIGHT_HIGHLIGHTMANAGER_H
