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
#include <ves/conductor/qt/scriptingTools/SquirrelConnection.h>

#if SWITCHWIRE_HAVE_SQUIRREL
#include <ves/conductor/qt/scriptingTools/SquirrelUtilClasses.h>

#include <switchwire/squirrel/ConnectScripts.h>
#include <switchwire/squirrel/ExposeSignals.h>
#include <switchwire/squirrel/SQStdMap.h>
#include <switchwire/squirrel/SQStdVector.h>

#include <squirrel.h>
//DIAG_OFF(unused-parameter)
#include <sqrat.h>
#include <sqrat/sqratVM.h>
//DIAG_ON(unused-parameter)

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
SquirrelConnection::SquirrelConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SquirrelConnection::SquirrelConnection( const std::string& scriptText )
{
    runScript( scriptText );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::ExposeSignalTypes( Sqrat::SqratVM& vm )
{
    // Add signal types to expose to script engine.
    // Naming scheme for signal types should mimic that in
    // ves/util/SimpleDataTypeSignalSignatures.h, but omitting "_type" at the
    // end. Non-void return types should be prepended to the name, e.g.
    // Bool_IntSignal to describe a signal with signature bool( int ).

    ExposeSignalType_0< void( ) >( "VoidSignal", vm );
    ExposeSignalType_1< void( const std::string& ) >( "StringSignal", vm );
    ExposeSignalType_2< void( const std::string&, const std::string& ) >( "TwoStringSignal", vm );
    ExposeSignalType_3< void ( std::string const&, std::string const&, std::string const& ) >( "ThreeStringSignal", vm );
    ExposeSignalType_2< void( const std::string&, const bool& ) >( "StringBoolSignal", vm );
    ExposeSignalType_2< void( const std::string&, int ) >( "StringIntSignal", vm );
    ExposeSignalType_1< void ( bool const& ) >( "BoolSignal", vm );
    ExposeSignalType_1< void ( double const& ) >( "DoubleSignal", vm );
    ExposeSignalType_2< void ( double const&, double const& ) >( "TwoDoubleSignal", vm );
    ExposeSignalType_3< void ( double const&, double const&, double const& ) >( "ThreeDoubleSignal", vm );
    ExposeSignalType_1< void ( int const& ) >( "IntSignal", vm );
    ExposeSignalType_1< void ( unsigned int const& ) >( "UnsignedIntSignal", vm );
    ExposeSignalType_2< void ( const bool, const std::vector< double >& ) >( "BoolAndDoubleVectorSignal", vm );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::BindSpecialClasses()
{
    // Add any other special classes that need to be bound to the bottom of
    // this method

    // Add any other vector types required to the bottom of this block
    BindSQStdVector< int >( "IntVector" );
    BindSQStdVector< float >( "FloatVector" );
    BindSQStdVector< double >( "DoubleVector" );
    BindSQStdVector< std::string >( "StringVector" );

    // Add any other map types require to the bottom of this block
    BindSQStdMap< std::string, std::string >( "StringStringMap" );

    typedef VizPropertySetWrapper psw;
    Sqrat::Class< psw > psClass;
    psClass.Func( "CreateNewFeature", &psw::CreateNewFeature );
    psClass.Func( "SetBoolPropertyValue", &psw::SetBoolPropertyValue );
    psClass.Func( "SetIntPropertyValue", &psw::SetIntPropertyValue );
    psClass.Func( "SetFloatPropertyValue", &psw::SetFloatPropertyValue );
    psClass.Func( "SetDoublePropertyValue", &psw::SetDoublePropertyValue );
    psClass.Func( "SetStringPropertyValue", &psw::SetStringPropertyValue );
    psClass.Func( "Save", &psw::Save );
    psClass.Func( "BulkSave", &psw::BulkSave );
    psClass.Func( "GetUUIDAsString", &psw::GetUUIDAsString );
    Sqrat::RootTable().Bind( "VizPropertySet", psClass );

    Sqrat::Class< TweakStore > tsClass;
    tsClass.Func( "OpenTransaction", &TweakStore::OpenTransaction );
    tsClass.Func( "CloseTransaction", &TweakStore::CloseTransaction );
    Sqrat::RootTable().Bind( "TweakStore", tsClass );

    Sqrat::Class< Sleeper > sleepClass;
    sleepClass.Func( "Sleep", &Sleeper::Sleep );
    Sqrat::RootTable().Bind( "Sleeper", sleepClass );
}
////////////////////////////////////////////////////////////////////////////////
void SquirrelConnection::runScript( const std::string& scriptText )
{
    Sqrat::SqratVM vm;
    Sqrat::DefaultVM::Set(vm.getVM());

    ExposeSignalTypes( vm );
    BindSpecialClasses();

    try
    {
        Sqrat::Script script;
        script.CompileString( scriptText );
        script.Run();
    }
    catch( Sqrat::Exception& e )
    {
        //TODO: push these message out to a logger rather than stdout
        std::cout << "Sqrat exception: " << e.Message() << std::endl << std::flush;
        return;
    }
    catch( ... )
    {
        std::cout << "Unspecified Sqrat exception" << std::endl << std::flush;
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
}} //ves::conductor

#endif
