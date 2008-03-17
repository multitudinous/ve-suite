#ifndef VE_FERMENTOR_GRAPHICAL_PLUGIN_H
#define VE_FERMENTOR_GRAPHICAL_PLUGIN_H

// --- My Includes --- //
class Shaders;

namespace display
{
    class DigitalGauge;
}

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

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

// --- C/C++ Libraries --- //
#include <map>

class VE_USER_PLUGIN_EXPORTS VEFermentorGraphicalPlugin : public ves::xplorer::plugin::PluginBase
{
public:
    VEFermentorGraphicalPlugin();
    virtual ~VEFermentorGraphicalPlugin();

    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS );
    virtual void PreFrameUpdate(); 
    virtual void ProcessOnSubmitJob();
       
    void UpdateGauges( double, double, double, double, double, double, double );

private:
    ///Variable to let the plugin know when a simulation has jsut been changed
    bool mSimulationStart;
    
    int frame_count;
    int frame_speed_control;

    double _agitation;
    double _air_conc;
    double _ini_ph;
    double _nitrate_conc;
    double _temperature;
    double _hours;
    double _rot_speed;
    double _sim_speed;

    long _cycle_ID;
    long _rotation_ID;
    long _xray_ID;
    long _loop_ID;

    std::vector< double > time_steps;
    std::vector< double > result_steps;

    Shaders* shader;

    std::map< int, osg::ref_ptr< display::DigitalGauge > > _gauges;

    osg::ref_ptr< osg::Sequence > capsule_sequence;

    osg::ref_ptr< osg::Node > _fermentorGeometry;
    osg::ref_ptr< osg::Node > _impellerGeometry;
    osg::ref_ptr< osg::Node > _tankGeometry;

    osg::ref_ptr< osg::MatrixTransform > _roomGeometry;
    osg::ref_ptr< osg::MatrixTransform > fermentorGroup;

    osg::ref_ptr< osg::MatrixTransform > transform_ferm;
    osg::ref_ptr< osg::MatrixTransform > transform_imp;
    osg::ref_ptr< osg::MatrixTransform > transform_tank;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VEFermentorGraphicalPlugin )

#endif //VE_FERMENTOR_GRAPHICAL_PLUGIN_H
