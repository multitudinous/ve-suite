/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdDigitalAnalogGauge.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_DIGITALANALOGGAUGE_H
#define CFD_DIGITALANALOGGAUGE_H

//#include <vector>
//#include <string>

class cfdGroup;
class cfdNode;
class cfdReadParam;
#include "cfdDCS.h"
#include <utility>
#include <string>
class cfd1DTextInput;

class cfdDigitalAnalogGauge : public cfdDCS
{
   public:

      cfdDigitalAnalogGauge( cfdGroup* );
      //cfdDigitalAnalogGauge( cfdDigitalAnalogGauge* );

      ~cfdDigitalAnalogGauge( void );

      void SetGaugeName( std::string );
      void SetGeometryFilename( std::string );
      void SetModuleName( std::string );
      void SetDataValue( std::string );
      void SetUnitsTag( std::string );
      void SetDataTag( std::string );
      void CreateGaugeName( void );

      std::string GetModuleName( void );
      std::string GetDataTag( void );

      void Update( void );
      std::pair < cfd1DTextInput*, cfd1DTextInput* > _textOutput;
   private:
   
      cfdNode* node;
      cfdGroup* _masterNode;

      std::string _filename;
      std::string _gaugeName;
      std::string _moduleName;
      std::string _unitsName;
      std::string _gaugeTagName;
};

#endif
