
// --- CPSI Includes --- //
#include "CPSI.h"

// --- Powersim Includes --- //
#include "PsSimEng_i.c"

// --- C/C++ Includes --- //
#include <vector>

using namespace cpsi;

////////////////////////////////////////////////////////////////////////////////
Project::Project()
    //:
    //m_simulationEngine( NULL ),
    //m_simulationProject( NULL ),
    //m_simulation( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Project::~Project()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Project::Open(
    const ATL::CString& fileName,
    const ATL::CString& key,
    const ATL::CString& password )
{
    // ---------------------------------------------------------------------- //
    // Engine
    // ---------------------------------------------------------------------- //
    //Create instance of the SimulationEngine
    ATL::CComPtr< ISimulationEngine > m_simulationEngine;
    HRESULT hr = m_simulationEngine.CoCreateInstance(
        CLSID_SimulationEngine, NULL, CLSCTX_INPROC_SERVER );
    ATLASSERT( SUCCEEDED( hr ) );

    // ---------------------------------------------------------------------- //
    // Project - access to languages/global units/global ranges
    // ---------------------------------------------------------------------- //
    //Open a SimulationProject
    ATL::CComPtr< ISimulationProject > m_simulationProject;
    ATL::CComBSTR bstrFileName( fileName );
    ATL::CComBSTR bstrKey( key );
    ATL::CComBSTR bstrPassword( password );
    hr = m_simulationEngine->OpenProject(
        bstrFileName, bstrKey, bstrPassword,
        VARIANT_FALSE, VARIANT_TRUE, &m_simulationProject );
    ATLASSERT( SUCCEEDED( hr ) );

    //Get the Languages Collection
    ATL::CComPtr< ILanguages > languages;
    hr = m_simulationProject->get_Languages( &languages );
    ATLASSERT( SUCCEEDED( hr ) );

    long numLanguages( 0 );
    hr = languages->get_Count( &numLanguages );
    ATLASSERT( SUCCEEDED( hr ) );

    std::vector< ATL::CComPtr< ILanguage > > languageVector;
    for( long i = 0; i < numLanguages; ++i )
    {
        ATL::CComPtr< ILanguage > language;
        languageVector.push_back( language );
        hr = languages->get_Item( i, &language );
        ATLASSERT( SUCCEEDED( hr ) );
    }

    //Get the SimulationComponents Collection
    ATL::CComPtr< ISimulationComponents > simulationComponents;
    hr = m_simulationProject->get_Components( &simulationComponents );
    ATLASSERT( SUCCEEDED( hr ) );

    long numSimulationComponents( 0 );
    hr = simulationComponents->get_Count( &numSimulationComponents );
    ATLASSERT( SUCCEEDED( hr ) );

    std::vector< ATL::CComPtr< ISimulationComponent > > simulationComponentVector;
    for( long i = 0; i < numSimulationComponents; ++i )
    {
        // ------------------------------------------------------------------ //
        // Components - access to model variables/simulations/ranges/units
        // ------------------------------------------------------------------ //
        //Open a SimulationComponent
        ATL::CComPtr< ISimulationComponent > simulationComponent;
        simulationComponentVector.push_back( simulationComponent );
        hr = simulationComponents->get_Item(
            ATL::CComVariant( i ), &simulationComponent );
        ATLASSERT( SUCCEEDED( hr ) );

        /*
        //Get the ModelVariables Collection for all simulations
        //You will not get access to the variable values from this collection
        ATL::CComPtr< IModelVariables > modelVariables;
        hr = simulationComponent->get_Variables( &modelVariables );
        ATLASSERT( SUCCEEDED( hr ) );

        long numModelVariables( 0 );
        hr = modelVariables->get_Count( &numModelVariables );
        ATLASSERT( SUCCEEDED( hr ) );

        std::vector< ATL::CComPtr< IModelVariable > > modelVariableVector;
        for( long j = 0; j < numModelVariables; ++j )
        {
            ATL::CComPtr< IModelVariable > modelVariable;
            modelVariableVector.push_back( modelVariable );
            hr = modelVariables->get_Item( ATL::CComVariant( j ), &modelVariable );
            ATLASSERT( SUCCEEDED( hr ) );
        }
        */

        //Get the Simulations Collection
        ATL::CComPtr< ISimulations > simulations;
        hr = simulationComponent->get_Simulations( &simulations );
        ATLASSERT( SUCCEEDED( hr ) );

        long numSimulations( 0 );
        hr = simulations->get_Count( &numSimulations );
        ATLASSERT( SUCCEEDED( hr ) );

        std::vector< ATL::CComPtr< ISimulation > > simulationVector;
        for( long j = 0; j < numSimulations; ++j )
        {
            // -------------------------------------------------------------- //
            // Variables and Values - access to value/value history/model variables
            // -------------------------------------------------------------- //
            ATL::CComPtr< ISimulation > simulation;
            simulationVector.push_back( simulation );
            hr = simulations->get_Item( ATL::CComVariant( j ), &simulation );
            ATLASSERT( SUCCEEDED( hr ) );

            //Get the ModelVariables Collection
            ATL::CComPtr< IModelVariables > modelVariables;
            hr = simulation->get_Variables( &modelVariables );
            ATLASSERT( SUCCEEDED( hr ) );

            long numModelVariables( 0 );
            hr = modelVariables->get_Count( &numModelVariables );
            ATLASSERT( SUCCEEDED( hr ) );

            std::vector< ATL::CComPtr< IModelVariable > > modelVariableVector;
            for( long k = 0; k < numModelVariables; ++k )
            {
                ATL::CComPtr< IModelVariable > modelVariable;
                modelVariableVector.push_back( modelVariable );
                hr = modelVariables->get_Item( ATL::CComVariant( k ), &modelVariable );
                ATLASSERT( SUCCEEDED( hr ) );

                //Get the Dimensions Collection
                ATL::CComPtr< IDimensions > dimensions;
                hr = modelVariable->get_Dimensions( &dimensions );
                ATLASSERT( SUCCEEDED( hr ) );

                long numDimensions( 0 );
                hr = dimensions->get_Count( &numDimensions );
                ATLASSERT( SUCCEEDED( hr ) );

                std::vector< ATL::CComPtr< IDimension > > dimensionVector;
                for( long l = 0; l < numDimensions; ++l )
                {
                    ATL::CComPtr< IDimension > dimension;
                    dimensionVector.push_back( dimension );
                    hr = dimensions->get_Item( l, &dimension );
                    ATLASSERT( SUCCEEDED( hr ) );
                }

                //Get the Value
                ATL::CComPtr< IValue > value;
                //hr = modelVariable->get_Value(  );
                //ATLASSERT( SUCCEEDED( hr ) );

                //Get the ValueHistory
                ATL::CComPtr< IValueHistory > valueHistory;
                //hr = modelVariable->AcquireValueHistory(  );
                //ATLASSERT( SUCCEEDED( hr ) );
            }
        }

        //Get the Local Ranges Collection
        ATL::CComPtr< IRanges > localRanges;
        hr = simulationComponent->get_Ranges( &localRanges );
        ATLASSERT( SUCCEEDED( hr ) );

        long numLocalRanges( 0 );
        hr = localRanges->get_Count( &numLocalRanges );
        ATLASSERT( SUCCEEDED( hr ) );

        std::vector< ATL::CComPtr< IRange > > localRangeVector;
        for( long j = 0; j < numLocalRanges; ++j )
        {
            ATL::CComPtr< IRange > localRange;
            localRangeVector.push_back( localRange );
            hr = localRanges->get_Item( ATL::CComVariant( j ), &localRange );
            ATLASSERT( SUCCEEDED( hr ) );
        }

        //Get the Local Units Collection
        ATL::CComPtr< IUnits > localUnits;
        hr = simulationComponent->get_Units( &localUnits );
        ATLASSERT( SUCCEEDED( hr ) );

        long numLocalUnits( 0 );
        hr = localUnits->get_Count( &numLocalUnits );
        ATLASSERT( SUCCEEDED( hr ) );

        std::vector< ATL::CComPtr< IUnit > > localUnitVector;
        for( long j = 0; j < numLocalUnits; ++j )
        {
            ATL::CComPtr< IUnit > localUnit;
            localUnitVector.push_back( localUnit );
            hr = localUnits->get_Item( ATL::CComVariant( j ), &localUnit );
            ATLASSERT( SUCCEEDED( hr ) );
        }
    }

    //Get the Global Ranges Collection
    ATL::CComPtr< IRanges > globalRanges;
    hr = m_simulationProject->get_Ranges( &globalRanges );
    ATLASSERT( SUCCEEDED( hr ) );

    long numGlobalRanges( 0 );
    hr = globalRanges->get_Count( &numGlobalRanges );
    ATLASSERT( SUCCEEDED( hr ) );

    std::vector< ATL::CComPtr< IRange > > globalRangeVector;
    for( long i = 0; i < numGlobalRanges; ++i )
    {
        ATL::CComPtr< IRange > globalRange;
        globalRangeVector.push_back( globalRange );
        hr = globalRanges->get_Item( ATL::CComVariant( i ), &globalRange );
        ATLASSERT( SUCCEEDED( hr ) );
    }

    //Get the Global Units Collection
    ATL::CComPtr< IUnits > globalUnits;
    hr = m_simulationProject->get_Units( &globalUnits );
    ATLASSERT( SUCCEEDED( hr ) );

    long numGlobalUnits( 0 );
    hr = globalUnits->get_Count( &numGlobalUnits );
    ATLASSERT( SUCCEEDED( hr ) );

    std::vector< ATL::CComPtr< IUnit > > globalUnitVector;
    for( long i = 0; i < numGlobalUnits; ++i )
    {
        ATL::CComPtr< IUnit > globalUnit;
        globalUnitVector.push_back( globalUnit );
        hr = globalUnits->get_Item( ATL::CComVariant( i ), &globalUnit );
        ATLASSERT( SUCCEEDED( hr ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
