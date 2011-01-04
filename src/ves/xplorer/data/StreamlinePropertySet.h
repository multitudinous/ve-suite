/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef VES_XPLORER_DATA_STREAMLINEPROPERTYSET_H
#define VES_XPLORER_DATA_STREAMLINEPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/PropertyPtr.h>

#include <ves/VEConfig.h>

#include <boost/signals2/signal.hpp>

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file StreamlinePropertySet.h
 *
 */

/*!\class ves::xplorer::data::StreamlinePropertySet
 *
 */

/*!\namespace ves::xplorer::data
 *
 */
class VE_DATA_EXPORTS StreamlinePropertySet : public PropertySet
{
public:
    ///Constructor
    StreamlinePropertySet();
    ///Copy Contructor
    StreamlinePropertySet( const StreamlinePropertySet& orig );
    ///Destructor
    virtual ~StreamlinePropertySet();

private:
    ///Update method
    void UpdateModeOptions( PropertyPtr property );
    ///Validate method
    bool ValidateScalarMinMax( PropertyPtr property, boost::any value );
    ///Update method
    void UpdateScalarDataOptions( PropertyPtr property );
    ///Update method
    void UpdateScalarDataRange( PropertyPtr property );
    ///Update method
    void UpdateVectorDataOptions( PropertyPtr property );

    /// Slot connected to value changes of properties SeedPoints_NumberOfPointsInX,
    /// SeedPoints_NumberOfPointsInY, and SeedPoints_NumberOfPointsInZ
    void UpdateNumberOfPoints( PropertyPtr property );
    ///Slot connected to the value change of  display seed points
    ///\param property The bool value for the seed point display flag
    void UpdateSeedPointDisplay( PropertyPtr property );

private:
    ///Create the skeleton
    void CreateSkeleton();

    ///Update signal containing new seed points dimensions
    typedef boost::signals2::signal< void ( const std::vector< long >& ) > UpdateDimsSignal_type;
    UpdateDimsSignal_type mUpdateDims;
    ///Update signal to control turning off and on seed points
    typedef boost::signals2::signal< void ( const std::string&, const bool ) > ActivateSeedPointsSignal_type;
    ActivateSeedPointsSignal_type m_activateSeedPoints;
    ///Update signal to control the bounding box for seed points
    typedef boost::signals2::signal< void ( const std::vector< double >& ) > UpdateSeedPointBoundsSignal_type;
    UpdateSeedPointBoundsSignal_type m_updateSeedPointBounds;
    ///Update signal to control changing the active dataset
    typedef boost::signals2::signal< void ( const std::string& ) > UpdateActiveDataSetSignal_type;
    UpdateActiveDataSetSignal_type m_activeDataSet;
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* VES_XPLORER_DATA_STREAMLINEPROPERTYSET_H */
