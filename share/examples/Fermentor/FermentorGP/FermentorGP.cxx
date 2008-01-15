// --- My Includes --- //
#include "FermentorGP.h"
#include "DigitalGauge.h"
#include "Shaders.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/Model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/Shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>

#include <osgSim/ColorRange>

// --- C/C++ Libraries --- //


////////////////////////////////////////////////////////////////////////////////
VEFermentorGraphicalPlugin::VEFermentorGraphicalPlugin()
:
cfdVEBaseClass(),
_agitation( 200 ),
_air_conc( 1.25 ),
_ini_ph( 6 ),
_nitrate_conc( 0.1 ),
_temperature( 37 ),
_hours( 240 ),
_cycle_ID( 0 ),
_rotation_ID( 0 ),
_xray_ID( 0 ),
_loop_ID( 0 ),
_rot_speed( 0 ),
_sim_speed( 0 ),
frame_count( 0 ),
frame_speed_control( 0 )
{
    m_objectName = "FermentorUI";

    capsule_sequence = new osg::Sequence();
    capsule_sequence->setValue( 0 );
    fermentorGroup = new osg::MatrixTransform();
    shader = new Shaders();
}
////////////////////////////////////////////////////////////////////////////////
VEFermentorGraphicalPlugin::~VEFermentorGraphicalPlugin()
{
    _gauges.clear();

    delete shader;
}
////////////////////////////////////////////////////////////////////////////////
void VEFermentorGraphicalPlugin::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    cfdVEBaseClass::InitializeNode( veworldDCS );

    //Create the gauges
    osg::ref_ptr< ves::xplorer::scenegraph::Group > rootNode =
        ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();

    osg::ref_ptr< osg::MatrixTransform > _roomGeometry = new osg::MatrixTransform();
    osg::ref_ptr< osg::Node > temp = osgDB::readNodeFile( "Models/fermentor_room.ive" );
    _roomGeometry->addChild( temp.get() );
    rootNode->addChild( _roomGeometry.get() );
    m_dcs->addChild( fermentorGroup.get() );

    _fermentorGeometry = osgDB::readNodeFile( "Models/fermentor_noimpeller.ive" );
    _impellerGeometry = osgDB::readNodeFile( "Models/impeller_fixed.ive" );
    _impellerGeometry->setStateSet( shader->Phong().get() );
    _tankGeometry = osgDB::readNodeFile( "Models/opaque_tank.ive" );
    _tankGeometry->setStateSet( shader->Phong().get() );

    transform_ferm = new osg::MatrixTransform();
    transform_imp = new osg::MatrixTransform();
    transform_tank = new osg::MatrixTransform();

    transform_ferm->addChild( _fermentorGeometry.get() );
    transform_imp->addChild( _impellerGeometry.get() );
    transform_tank->addChild( _tankGeometry.get() );

    fermentorGroup->addChild( transform_ferm.get() );
    fermentorGroup->addChild( transform_imp.get() );
    fermentorGroup->addChild( transform_tank.get() );

    double trans[ 3 ] = { 1, 12, 2 };
    m_dcs->SetTranslationArray( trans );

    _roomGeometry->setMatrix( osg::Matrix::scale( 3.28, 3.28, 3.28 ) *
                              osg::Matrix::translate( -4.5, 0.0, -3.4 ) *
                              osg::Matrix::rotate( 0.0, 0, 1, 0 ) );
    
    transform_ferm->setMatrix( osg::Matrix::scale( 3.28, 3.28, 3.28 ) *
                               osg::Matrix::translate( -0.67, 0.8, -1.36 ) );

    transform_tank->setMatrix( osg::Matrix::scale( 3.28, 3.28, 3.28 ) *
                               osg::Matrix::translate( 0.005, -0.02, -0.05 ) );

    _gauges.insert( std::make_pair( 0, new display::DigitalGauge( "Time: Hours" ) ) );
    _gauges.insert( std::make_pair( 1, new display::DigitalGauge( "Acid Yield" ) ) );
    _gauges.insert( std::make_pair( 2, new display::DigitalGauge( "Agitation: rpm" ) ) );
    _gauges.insert( std::make_pair( 3, new display::DigitalGauge( "Air Conc: vvm" ) ) );
    _gauges.insert( std::make_pair( 4, new display::DigitalGauge( "Initial pH" ) ) );
    _gauges.insert( std::make_pair( 5, new display::DigitalGauge( "Nitrate: g/L" ) ) );
    _gauges.insert( std::make_pair( 6, new display::DigitalGauge( "Temp: C" ) ) );

    for( std::map< int, osg::ref_ptr< display::DigitalGauge > >::iterator
            itr = _gauges.begin(); itr != _gauges.end(); ++itr )
    {
        rootNode->addChild( itr->second.get() );

        itr->second->GetNameText()->setCharacterSize( 1.5 );
        itr->second->GetDigitalText()->setCharacterSize( 1.5 );
        itr->second->GetNameText()->setColor( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
        itr->second->GetDigitalText()->setColor( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
    }

    _gauges[ 0 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate(  2.5, 6, 3.5 ) );
    _gauges[ 1 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate(  2.5, 6, 2.9 ) );
    _gauges[ 2 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate( -2.5, 6, 4.7 ) );
    _gauges[ 3 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate( -2.5, 6, 4.1 ) );
    _gauges[ 4 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate( -2.5, 6, 3.5 ) );
    _gauges[ 5 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate( -2.5, 6, 2.9 ) );
    _gauges[ 6 ]->setMatrix( osg::Matrix::scale( 12, 12, 12 ) *
                             osg::Matrix::translate( -2.5, 6, 2.3 ) );

    _gauges[ 0 ]->GetNameText()->setPosition(    osg::Vec3(  2.10, 6,  0.20 ) );
    _gauges[ 0 ]->GetDigitalText()->setPosition( osg::Vec3(  2.15, 6, -0.05 ) );
    _gauges[ 1 ]->GetNameText()->setPosition(    osg::Vec3(  2.10, 6, -0.40 ) );
    _gauges[ 1 ]->GetDigitalText()->setPosition( osg::Vec3(  2.15, 6, -0.65 ) );
    _gauges[ 2 ]->GetNameText()->setPosition(    osg::Vec3( -2.95, 6,  1.40 ) );
    _gauges[ 2 ]->GetDigitalText()->setPosition( osg::Vec3( -2.85, 6,  1.15 ) );
    _gauges[ 3 ]->GetNameText()->setPosition(    osg::Vec3( -2.95, 6,  0.80 ) );
    _gauges[ 3 ]->GetDigitalText()->setPosition( osg::Vec3( -2.85, 6,  0.55 ) );
    _gauges[ 4 ]->GetNameText()->setPosition(    osg::Vec3( -2.95, 6,  0.20 ) );
    _gauges[ 4 ]->GetDigitalText()->setPosition( osg::Vec3( -2.85, 6, -0.05 ) );
    _gauges[ 5 ]->GetNameText()->setPosition(    osg::Vec3( -2.95, 6, -0.40 ) );
    _gauges[ 5 ]->GetDigitalText()->setPosition( osg::Vec3( -2.85, 6, -0.65 ) );
    _gauges[ 6 ]->GetNameText()->setPosition(    osg::Vec3( -2.95, 6, -1.00 ) );
    _gauges[ 6 ]->GetDigitalText()->setPosition( osg::Vec3( -2.85, 6, -1.25 ) );
}
////////////////////////////////////////////////////////////////////////////////
void VEFermentorGraphicalPlugin::ProcessOnSubmitJob()
{
    m_xmlModel->GetInput( "agitation" )->GetDataValuePair( "agitation" )->GetData( _agitation );
    m_xmlModel->GetInput( "air_conc" )->GetDataValuePair( "air_conc" )->GetData( _air_conc );
    m_xmlModel->GetInput( "ini_ph" )->GetDataValuePair( "ini_ph" )->GetData( _ini_ph );
    m_xmlModel->GetInput( "nitrate_conc" )->GetDataValuePair( "nitrate_conc" )->GetData( _nitrate_conc );
    m_xmlModel->GetInput( "temperature" )->GetDataValuePair( "temperature" )->GetData( _temperature );
    m_xmlModel->GetInput( "hours" )->GetDataValuePair( "hours" )->GetData( _hours );
    m_xmlModel->GetInput( "cycle_ID" )->GetDataValuePair( "cycle_ID" )->GetData( _cycle_ID );
    m_xmlModel->GetInput( "rotation_ID" )->GetDataValuePair( "rotation_ID" )->GetData( _rotation_ID );
    m_xmlModel->GetInput( "xray_ID" )->GetDataValuePair( "xray_ID" )->GetData( _xray_ID );
    m_xmlModel->GetInput( "loop_ID" )->GetDataValuePair( "loop_ID" )->GetData( _loop_ID );
    m_xmlModel->GetInput( "rot_speed" )->GetDataValuePair( "rot_speed" )->GetData( _rot_speed );
    m_xmlModel->GetInput( "sim_speed" )->GetDataValuePair( "sim_speed" )->GetData( _sim_speed );

    _rot_speed = _rot_speed / 10.0f;

    if( _sim_speed > 0.0f )
    {
        _sim_speed = 1 / _sim_speed;
    }

    std::fstream results;                                      //File for statistical results

    results.open( "results.txt", std::ios::out ); 

    //Biochemical reaction equations
    double c[ 8 ];
    double speed = 0;
    double min = 1000000000;
    double max = -1000000000;

    time_steps.clear();
    result_steps.clear();

    if( _rotation_ID == 0 )
    {
        fermentorGroup->setUpdateCallback( new osg::AnimationPathCallback(
            osg::Vec3( 0, 0, 0 ), osg::Z_AXIS, 0.0f ) );
    }
    else if( _rotation_ID == 1 )
    {
        fermentorGroup->setUpdateCallback( new osg::AnimationPathCallback(
            osg::Vec3( 0, 0, 0 ), osg::Z_AXIS, _rot_speed ) );
    }

    if( _cycle_ID == 0 )
    {
        time_steps.push_back( 0 );
        result_steps.push_back( 0 );

        capsule_sequence->removeChildren( 0, static_cast< int >( 
            capsule_sequence->getNumChildren() ) );
        capsule_sequence->setMode( osg::Sequence::STOP );
    }
    else if( _cycle_ID == 1 )
    {
        capsule_sequence->removeChildren( 0, static_cast< int >( 
            capsule_sequence->getNumChildren() ) );
        capsule_sequence->setMode( osg::Sequence::START );

        results << "Agitation(rpm):\t" << _agitation << "\n";
        results << "Air Conc(vvm):\t" << _air_conc << "\n";
        results << "Initial pH:\t" << _ini_ph << "\n";
        results << "Nitrate(g/L):\t" << _nitrate_conc << "\n";
        results << "Temp(C): \t" << _temperature << "\n\n";

        results << "t (hours):\t\t";
        results << "Acid Yield:\n";

        for( int t = 0; t <= _hours; ++t )
        {
            c[ 0 ] = 1;
            c[ 1 ] = ( -0.000036 * t * t * t ) + ( 0.0092 * t * t ) - ( 0.072 * t ) + 1;    
            c[ 2 ] = ( -0.000091 * _agitation * _agitation ) + 0.035 * _agitation - 2.56;
            c[ 3 ] = ( -1 * _air_conc * _air_conc ) + ( 2 * _air_conc ) - 2;
            c[ 4 ] = ( -0.41 * _ini_ph * _ini_ph ) + ( 4.9 * _ini_ph ) - 13;
            c[ 5 ] = ( -17 * _nitrate_conc * _nitrate_conc ) + ( 8.4 * _nitrate_conc ) - 0.004;
            c[ 6 ] = ( -0.01 * _temperature * _temperature ) + ( 0.69 * _temperature ) - 7.8;
            c[ 7 ] = -1;

            for( int i = 1; i < 8; ++i )
            {
                c[ 0 ] = c[ 0 ] * c[ i ];
            }

            if( c[ 0 ] <= 0 )
            {
                c[ 0 ] = 0.0;
            }

            if( c[ 0 ] < min )
            {
                min = c[ 0 ];
            }

            if( c[ 0 ] > max )
            {
                max = c[ 0 ];
            }

            results << t << "\t\t\t";
            results << c[ 0 ] << "\n";
            time_steps.push_back( t );
            result_steps.push_back( c[ 0 ] );
        }

        if( max == min )
        {
            min = 0.0f;
            max = 0.0000001;
        }

        //Create a custom color set
        std::vector< osg::Vec4 > cs;
        cs.push_back( osg::Vec4( 0.0f, 0.0f, 1.0f, 0.4f ) );   //Blue
        cs.push_back( osg::Vec4( 0.0f, 1.0f, 1.0f, 0.4f ) );   //Cyan
        cs.push_back( osg::Vec4( 0.0f, 1.0f, 0.0f, 0.4f ) );   //Green
        cs.push_back( osg::Vec4( 1.0f, 1.0f, 0.0f, 0.4f ) );   //Yellow
        cs.push_back( osg::Vec4( 1.0f, 0.0f, 0.0f, 0.4f ) );   //Red

        osg::ref_ptr< osgSim::ColorRange > cr = new osgSim::ColorRange( min, max, cs );

        for( int t = 0; t <= _hours; ++t )
        {
            //Create concentration color capsules
            osg::ref_ptr< osg::Geode > geode_0 = new osg::Geode;

            osg::ref_ptr< osg::Capsule > capsule = new osg::Capsule( osg::Vec3( 0.0, 0.0, 1.95 ), 0.75, 2.7 );
            osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
            osg::ref_ptr< osg::ShapeDrawable > sd = new osg::ShapeDrawable( capsule.get(), hints.get() );

            hints->setDetailRatio( 1.0f );

            sd->setColor( cr->getColor( result_steps.at( t ) ) );

            osg::ref_ptr< osg::StateSet > stateset_0 = new osg::StateSet();
            stateset_0->setMode( GL_BLEND, osg::StateAttribute::ON );
            stateset_0->setRenderingHint( osg::StateSet::OPAQUE_BIN );
            sd->setStateSet( stateset_0.get() );

            geode_0->addDrawable( sd.get() );

            capsule_sequence->addChild( geode_0.get() );

            capsule_sequence->setTime( capsule_sequence->getNumChildren() - 1, _sim_speed );
        }

        double _imp_speed = 0.0f;
        if( _sim_speed > 0.0f )
        {
            _imp_speed = _agitation / 30.0f;
        }

        transform_imp->setUpdateCallback( new osg::AnimationPathCallback(
            osg::Vec3( 0, 0, 0 ), osg::Z_AXIS, _imp_speed ) );
    }

    if( _sim_speed == 0 )
    {
        capsule_sequence->setMode( osg::Sequence::PAUSE );
    }

    results.close();

    //Loop through all children
    capsule_sequence->setInterval( osg::Sequence::LOOP, 0, -1 );

    //Real-time playback, repeat for a set number of reps
    if( _loop_ID == 0 )
    {
        capsule_sequence->setDuration( 1.0f, 1 );
    }

    //Real-time playback, repeat indefinitively
    if( _loop_ID == 1 )
    {
        capsule_sequence->setDuration( 1.0f, -1 );
    }

    if( _xray_ID == 0 )
    {
        _tankGeometry->setStateSet( shader->Phong().get() );
    }
    else if( _xray_ID == 1 )
    {
        osg::ref_ptr< osg::StateSet > stateset_1 = new osg::StateSet();
        stateset_1->setMode( GL_BLEND, osg::StateAttribute::ON );
        stateset_1->setRenderingHint( osg::StateSet::OPAQUE_BIN );
        _tankGeometry->setStateSet( stateset_1.get() );
        _tankGeometry->setStateSet( shader->XRay().get() );
    }

    fermentorGroup->addChild( capsule_sequence.get() );
}
////////////////////////////////////////////////////////////////////////////////
void VEFermentorGraphicalPlugin::PreFrameUpdate()
{
    int frame_rate = 1;
    if( ( frame_speed_control >= frame_rate )
        ||
        ( frame_speed_control < 0 ) )
    {
        frame_speed_control = 0;
    }
    frame_speed_control++;

    if( frame_speed_control == frame_rate )
    {
        if( time_steps.empty() || result_steps.empty() )
        {
            return;
        }
        frame_count++;

        if( frame_count >= static_cast< int >( time_steps.size() ) || frame_count < 0 )
        { 
            frame_count=0;
        }

        if( frame_count < static_cast< int >( time_steps.size() ) )
        {
            if( capsule_sequence->getValue() != -1 )
            {
                UpdateGauges( time_steps[ capsule_sequence->getValue() ],
                              result_steps[ capsule_sequence->getValue() ],
                              _agitation,
                              _air_conc,
                              _ini_ph,
                              _nitrate_conc,
                              _temperature );
            }
        }

        if( capsule_sequence->getValue() == _hours &&
            _loop_ID == 0 || _cycle_ID == 0 )
        {
            transform_imp->setUpdateCallback(
                new osg::AnimationPathCallback( osg::Vec3( 0, 0, 0 ), osg::Z_AXIS, 0.0f ) );
        }

        frame_speed_control = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void VEFermentorGraphicalPlugin::UpdateGauges( double time_for_update,
                                               double result_for_update,
                                               double agitation,
                                               double air_conc,
                                               double ini_ph,
                                               double nitrate_conc,
                                               double temperature )
{
    _gauges[ 0 ]->UpdateText( time_for_update );
    _gauges[ 1 ]->UpdateText( result_for_update );
    _gauges[ 2 ]->UpdateText( agitation );
    _gauges[ 3 ]->UpdateText( air_conc );
    _gauges[ 4 ]->UpdateText( ini_ph );
    _gauges[ 5 ]->UpdateText( nitrate_conc );
    _gauges[ 6 ]->UpdateText( temperature );
}
////////////////////////////////////////////////////////////////////////////////
