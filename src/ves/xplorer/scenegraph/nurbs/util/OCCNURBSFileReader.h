/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef OCC_NURBS_FILE_READER_H
#define OCC_NURBS_FILE_READER_H
/*!\file OCCNURBSFileReader.h
 * OCCNURBSFileReader API
 */
/*!\file OCCNURBSFileReader.cxx
 * OCCNURBSFileReader Code
 */
/*!\namespace ves::xplorer::scenegraph::nurbs::util
 * NURBS Utilities API namespace
 */
/*!\class ves::xplorer::scenegraph::nurbs::util::OCCNURBSFileReader
 * Class that reads a NURBS patch file
 * Created by the Star-2-OCC utility
 */
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
class NURBSSurface;
}
}
}
}

#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
namespace util
{
///???
class VE_NURBS_UTILS_EXPORTS OCCNURBSFileReader
{
public:
    ///Constructor
    OCCNURBSFileReader();

    ///Destructor
    virtual ~OCCNURBSFileReader();

    ///Read in a NURBS patch created by the NURBSPointCreator utility.
    ///
    ///The user is responsible for cleaning up the memory associated with
    ///the returned ves::xplorer::scenegraph::nurbs::NURBSSurface.
    ///\param star2occFile ???
    ves::xplorer::scenegraph::nurbs::NURBSSurface* ReadPatchFile( std::string star2occFile );

protected:
    std::string _patchFile;///<The original star-to-occ data file.
    ves::xplorer::scenegraph::nurbs::NURBSSurface* _surfacePatch;///<The surface patch
};
}
}
}
}
}

#endif //OCC_NURBS_FILE_READER_H

