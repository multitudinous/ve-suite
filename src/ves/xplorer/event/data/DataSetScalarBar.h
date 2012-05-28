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
#ifndef DATASET_SCALARBAR_H
#define DATASET_SCALARBAR_H

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <osgSim/ScalarsToColors>
#include <osgSim/ColorRange>
#include <osgSim/ScalarBar>
using namespace osgSim;
using osgSim::ScalarBar;

#include <osg/ref_ptr>
#include <osg/Geode>

#include <string>
#include <vector>
#include <sstream>
#include <iostream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
}
}
}


namespace ves
{
namespace xplorer
{
/*!\file DataSetScalarBar.h
 * DataSetScalarBar API
 * class ves::xplorer::DataSetScalarBar
 * Renders an axis for a given dataset
 */
class VE_XPLORER_EXPORTS DataSetScalarBar : public GlobalBase
{
public:
    ///Constructor
    DataSetScalarBar( void );
    ///Destructor
    virtual ~DataSetScalarBar( void );
    /// in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {
        ;
    }
    /// Create the scalar bar
    void SetBoundingBox( double* inBBox );
    /// cleanup and add the scalar bar to the dcs
    void AddScalarBarToGroup();
    /// Get the scalar bar that was created
    ves::xplorer::scenegraph::DCS* GetScalarBar();
    ///Create the osg scalar bar
    osgSim::ScalarBar* CreateScalarBar();
    // Create a custom scalar printer
    struct MyScalarPrinter: public ScalarBar::ScalarPrinter
    {
        double min, max, mid;
        void SetMinMax( double minIn, double maxIn )
        {
            min = minIn;
            max = maxIn;

            mid = min + ( ( max - min ) * 0.5f );
        }

        std::string printScalar( float scalar )
        {
            //std::cout<<"In MyScalarPrinter::printScalar "<< scalar <<std::endl;
            std::ostringstream numStream;
            if( ( ( scalar > ( min - ( min * 0.001f ) ) ) &&
                    ( scalar < ( min + ( min * 0.001f ) ) ) ) ||
                    ( scalar == min ) )
            {
                numStream << min;
                return numStream.str();
            }
            else if( ( scalar > ( mid - ( mid * 0.001f ) ) ) &&
                     ( scalar < ( mid + ( mid * 0.001f ) ) ) )
            {
                numStream << mid;
                return numStream.str();
            }
            else if( ( scalar > ( max - ( max * 0.001f ) ) ) &&
                     ( scalar < ( max + ( max * 0.001f ) ) ) )
            {
                numStream << max;
                return numStream.str();
            }
            else
            {
                return " ";
            }
            //std::cout << ScalarBar::ScalarPrinter::printScalar(scalar) << std::endl;
        }
    };

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > scalarBarDCS; ///<Scalar bar coordinate system
    double bbox[ 6 ]; ///<Bounding box
};
}
}
#endif //DATASET_SCALARBAR_H
