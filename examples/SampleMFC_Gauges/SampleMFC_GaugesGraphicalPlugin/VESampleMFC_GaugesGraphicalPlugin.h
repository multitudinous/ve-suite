/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdVEBaseClass.h,v $
 * Date modified: $Date: 2004-08-28 09:40:47 -0700 (Sat, 28 Aug 2004) $
 * Version:       $Rev: 854 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SampleMFC_GaugesGraphicalPlugin_H
#define VE_SampleMFC_GaugesGraphicalPlugin_H

#include "VE_Xplorer/cfdVEBaseClass.h"

namespace VE_SceneGraph
{
   class cfdGroup;
   class cfdGeode;
}

namespace VE_Xplorer
{
   class cfdDigitalAnalogGauge;
}


class VE_USER_PLUGIN_EXPORTS VESampleMFC_GaugesGraphicalPlugin: public VE_Xplorer::cfdVEBaseClass 
{
public:
   VESampleMFC_GaugesGraphicalPlugin( void );
   ~VESampleMFC_GaugesGraphicalPlugin( void );

   virtual void InitializeNode( VE_SceneGraph::cfdDCS* );
   virtual void CreateCustomVizFeature( int input );
private:
   VE_Xplorer::cfdDigitalAnalogGauge* gauge;

   double calc1;
   double calc2;
   double calc3;

};
extern "C"
{

VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
{
   return new VESampleMFC_GaugesGraphicalPlugin();
}

}


#endif
   

