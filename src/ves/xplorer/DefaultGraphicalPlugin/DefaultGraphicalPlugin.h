/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DEFAULT_GRAPHICAL_PLUGIN_H
#define DEFAULT_GRAPHICAL_PLUGIN_H
/*!\file DefaultGraphicalPlugin.h
DefaultGraphicalPlugin API
*/

/*!\class ::DefaultGraphicalPlugin
*
*/
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

#include <ves/xplorer/scenegraph/DCS.h>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_SceneGraph
{
	class DCS;
}

namespace ves
{
namespace xplorer
{
namespace DefaultGraphicalPlugin
{
class VE_XPLORER_NETWORK_EXPORTS DefaultGraphicalPlugin : public ves::xplorer::plugin::cfdVEBaseClass 
{
public:
   DefaultGraphicalPlugin( void );
   virtual ~DefaultGraphicalPlugin( void );

	virtual void InitializeNode( VE_SceneGraph::DCS* veworldDCS );
private:
};
}
}
}
//VE_GRAPHICALPLUGIN_CLASS( DefaultGraphicalPlugin );

#endif // DEFAULT_GRAPHICAL_PLUGIN_H
   
