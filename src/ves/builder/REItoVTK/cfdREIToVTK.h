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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _CFD_REI_TRANSLATOR_H_
#define _CFD_REI_TRANSLATOR_H_


#include <ves/builder/cfdTranslatorToVTK/cfdTranslatorToVTK.h>
#include <set>

namespace VE_Builder
{
class /*VE_BUILDER_EXPORTS*/ cfdREItoVTK:
            public VE_Builder::cfdTranslatorToVTK
{

public:
    cfdREItoVTK();
    virtual ~cfdREItoVTK();
    std::string Getdb_file();
    std::string Getpp1_file();
    std::string Getpp3_file();
    std::string Getdbv_file();
    std::string Getppv_file();
    std::string Getpdv_file();
    std::string Getwrv_file();
    std::set<std::string> Getscalars();
    void Setdb_file( std::string );
    void Setpp1_file( std::string );
    void Setpp3_file( std::string );
    void Setdbv_file( std::string );
    void Setppv_file( std::string );
    void Setpdv_file( std::string );
    void Setwrv_file( std::string );
    void Setscalars( std::set<std::string> );

class REITranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback
    {
    public:
        REITranslateCbk()
        {};
        virtual ~REITranslateCbk()
        {};
        //////////////////////////////////////////////////
        //ouputDataset should be populated              //
        //appropriately by the translate callback.      //
        //////////////////////////////////////////////////
        virtual void Translate( vtkDataSet*& outputDataset,
                                cfdTranslatorToVTK* toVTK );
    protected:
    };
class REIPreTranslateCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback
    {
    public:
        REIPreTranslateCbk()
        {};
        virtual ~REIPreTranslateCbk()
        {};
        void Preprocess( int argc, char** argv, VE_Builder::cfdTranslatorToVTK* toVTK );
    protected:
    };
protected:
    REIPreTranslateCbk _cmdParser;
    REITranslateCbk _reiToVTK;
    std::string db_file;
    std::string pp1_file;
    std::string pp3_file;
    std::string dbv_file;
    std::string ppv_file;
    std::string pdv_file;
    std::string wrv_file;
    std::set<std::string> scalars;
};

}
#endif//_CFD_REI_TRANSLATOR_H_
