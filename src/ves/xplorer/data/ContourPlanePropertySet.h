/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef _CONTOURPLANEPROPERTYSET_H
#define	_CONTOURPLANEPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>

#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace data
{
class Property;
/*!\file ContourPlanePropertySet.h
 *
 */

/*!\class ves::xplorer::data::ContourPlanePropertySet
 *
 */

/*!\namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS ContourPlanePropertySet : public PropertySet
{
public:
    ///Constructor
    ContourPlanePropertySet();
    ///Copy Contructor
    ContourPlanePropertySet( const ContourPlanePropertySet& orig );
    ///Destructor
    virtual ~ContourPlanePropertySet();

private:
    ///Update method
    void UpdateModeOptions( Property* property );
    ///Validate method
    bool ValidateScalarMinMax( Property* property, boost::any value );
    ///Update method
    void UpdateScalarDataOptions( Property* property );
    ///Update method
    void UpdateScalarDataRange( Property* property );
    ///Update method
    void UpdateVectorDataOptions( Property* property );


private:
    ///Create the skeleton
    void CreateSkeleton();
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _CONTOURPLANEPROPERTYSET_H */

