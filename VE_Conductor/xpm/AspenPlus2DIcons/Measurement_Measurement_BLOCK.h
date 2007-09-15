#ifndef GETVESUITE_Measurement_Measurement_BLOCK_H
#define GETVESUITE_Measurement_Measurement_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Measurement_Measurement_BLOCK( void )
{
    unsigned char osgData[ 4305 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,96,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,142,191,98,95,248,39,183,236,11,241,199,225,135,198,63,138,95,26,255,0,97,239,217,3,227,7,196,223,20,126,223,255,0,240,84,255,0,248,73,190,34,252,82,253,154,126,11,252,64,241,223,136,191,177,63,224,166,191,181,199,135,116,111,237,223,23,120,179,193,87,122,134,175,246,79,15,233,26,85,141,175,218,46,36,251,61,158,153,111,107,22,200,33,141,23,235,255,0,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,130,105,255,0,201,186,252,70,255,0,179,255,0,255,0,130,177,127,235,211,127,108,138,251,254,128,62,0,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,143,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,107,239,250,40,3,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,190,255,0,162,128,62,0,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,143,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,107,239,250,40,3,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,190,255,0,162,128,62,0,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,190,32,255,0,130,155,127,193,50,127,224,155,126,2,255,0,130,109,255,0,193,65,188,117,224,95,248,39,207,236,65,224,191,27,120,47,246,32,253,171,252,89,224,239,24,248,79,246,80,248,13,225,207,21,120,79,197,94,28,248,13,227,237,99,195,222,38,240,207,136,116,127,0,195,119,160,248,130,195,87,179,179,186,178,189,181,154,43,155,91,155,88,231,130,68,149,21,135,238,245,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,212,0,127,193,52,255,0,228,221,126,35,127,217,255,0,255,0,193,88,191,245,233,191,182,69,125,255,0,95,0,127,193,52,255,0,228,221,126,35,127,217,255,0,255,0,193,88,191,245,233,191,182,69,125,255,0,64,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,213,247,253,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,212,0,127,193,52,255,0,228,221,126,35,127,217,255,0,255,0,193,88,191,245,233,191,182,69,125,255,0,95,0,127,193,52,255,0,228,221,126,35,127,217,255,0,255,0,193,88,191,245,233,191,182,69,125,255,0,64,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,213,247,253,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,212,0,127,193,52,255,0,228,221,126,35,127,217,255,0,255,0,193,88,191,245,233,191,182,69,125,255,0,95,152,31,240,72,255,0,139,63,11,62,48,126,203,191,19,124,69,240,147,226,95,195,255,0,138,62,31,181,253,191,255,0,224,168,31,105,215,62,29,120,203,195,190,54,209,173,255,0,225,47,255,0,130,137,126,211,127,20,188,39,231,234,158,25,212,174,160,135,251,83,225,151,196,15,1,120,143,78,220,227,237,186,15,141,180,141,94,219,205,211,245,59,59,137,253,126,231,254,10,107,255,0,4,220,179,240,182,141,227,171,207,248,40,55,236,65,107,224,159,17,248,131,196,222,18,240,255,0,140,110,127,106,255,0,128,208,120,87,93,241,87,130,244,239,9,235,30,49,240,206,141,226,9,124,124,45,53,79,16,105,58,71,143,124,11,117,169,217,193,52,151,54,22,254,52,210,103,186,138,40,181,43,54,152,3,238,10,43,199,252,51,251,66,252,2,241,167,193,171,159,218,47,193,191,28,126,15,248,179,246,124,178,240,255,0,139,60,89,119,241,219,195,95,18,252,25,174,252,27,181,240,183,128,103,214,109,124,117,226,107,175,137,250,94,181,46,137,7,135,244,91,175,14,120,130,61,90,245,175,133,182,157,38,131,122,151,146,194,214,179,136,247,254,40,252,89,248,89,240,59,192,154,239,197,31,141,127,18,254,31,252,31,248,103,225,127,236,207,248,73,190,34,252,82,241,151,135,62,31,248,19,195,191,219,122,206,159,225,221,27,251,119,197,222,44,212,108,244,253,39,237,126,32,213,244,171,27,95,180,92,71,246,139,205,78,222,214,45,211,207,26,48,7,160,209,95,47,183,237,187,251,23,166,163,240,223,71,127,218,243,246,96,93,91,227,31,136,52,15,9,252,34,210,219,227,239,194,129,169,124,84,241,79,139,60,43,240,199,199,94,21,240,207,195,139,19,226,191,55,199,30,32,212,252,17,241,183,224,206,179,167,217,105,105,117,115,123,165,124,92,240,198,161,109,28,150,154,254,149,53,223,63,226,79,248,40,87,236,11,224,223,248,79,191,225,46,253,184,127,100,15,10,255,0,194,169,248,129,101,240,155,226,143,252,36,127,180,183,193,141,19,254,21,175,197,77,72,248,215,251,63,225,167,143,255,0,180,252,107,23,252,33,222,63,159,254,21,183,196,95,39,70,212,126,205,168,203,255,0,8,14,183,229,219,55,246,85,247,144,1,246,5,21,241,255,0,252,60,39,246,5,31,11,63,225,122,127,195,112,254,200,31,240,164,191,225,96,127,194,166,255,0,133,197,255,0,13,45,240,99,254,21,103,252,45,49,225,207,248,76,63,225,90,127,194,194,255,0,132,215,251,39,254,22,7,252,34,95,241,52,254,198,251,95,246,151,246,119,250,119,217,190,203,251,218,60,57,255,0,5,9,253,129,124,99,255,0,8,8,240,143,237,195,251,32,120,171,254,22,183,143,239,126,19,252,45,255,0,132,115,246,150,248,49,174,127,194,202,248,167,166,255,0,194,21,253,163,240,211,192,63,217,158,53,151,254,19,31,136,16,127,194,201,248,117,231,104,218,119,218,117,24,191,225,63,209,55,219,47,246,173,143,158,1,246,5,21,227,255,0,17,255,0,104,95,128,95,7,60,83,240,199,192,191,23,126,56,124,32,248,87,227,127,141,158,32,111,9,252,25,240,119,196,127,137,126,12,240,63,138,190,46,120,169,53,47,15,232,239,225,175,134,62,30,241,62,181,107,119,227,221,124,106,254,44,240,181,169,179,210,161,187,184,23,30,37,211,224,49,249,151,150,203,39,207,254,25,255,0,130,155,127,193,55,60,105,169,92,232,254,14,255,0,130,131,126,196,62,44,213,236,188,63,226,223,22,93,233,126,26,253,172,62,3,107,186,141,175,133,124,5,225,93,103,199,94,58,241,53,205,142,149,227,233,100,131,195,250,47,130,124,57,226,29,103,86,189,100,22,218,110,149,160,222,234,55,146,67,105,107,60,200,1,247,5,124,1,255,0,5,98,255,0,148,89,127,193,75,63,236,192,63,108,143,253,103,95,136,213,239,250,223,237,99,251,44,248,103,254,20,103,252,36,127,180,183,192,15,15,255,0,195,80,127,100,127,195,52,127,109,252,100,248,117,164,255,0,195,68,127,111,255,0,194,47,253,131,255,0,10,47,237,254,35,143,254,22,223,219,191,225,56,240,87,216,255,0,176,63,180,62,211,255,0,9,134,151,228,239,254,208,180,243,191,40,63,224,166,191,240,83,95,248,38,231,143,191,224,155,159,240,80,111,2,248,23,254,10,15,251,16,120,215,198,222,52,253,136,63,106,255,0,9,120,59,193,190,19,253,171,254,3,120,143,197,94,44,241,87,136,254,3,120,247,71,240,247,134,188,51,225,253,31,199,211,93,235,222,32,191,213,239,44,237,108,172,173,97,150,230,234,230,234,56,32,142,73,100,68,32,31,156,63,0,254,44,111,240,71,237,71,165,126,199,95,18,255,0,225,49,211,127,224,169,63,180,7,252,20,27,246,112,208,190,44,254,205,30,51,251,117,143,236,231,251,83,92,255,0,193,91,254,52,248,55,75,253,167,52,157,75,225,110,165,113,31,196,127,218,3,80,255,0,130,124,126,221,94,24,248,215,2,195,168,120,107,196,83,124,26,255,0,130,89,88,106,177,235,242,248,38,11,127,19,124,62,60,45,251,79,254,203,63,14,255,0,103,31,248,39,54,159,251,59,126,217,31,176,7,236,229,225,255,0,2,127,193,111,191,224,173,159,240,173,188,93,227,189,103,225,215,139,127,103,31,134,31,11,52,219,95,248,43,132,250,30,239,135,94,6,248,247,240,246,8,254,31,183,195,47,218,19,246,125,181,208,126,193,226,157,23,78,176,139,246,128,248,123,125,15,219,52,253,119,68,211,181,159,223,239,248,38,159,252,155,175,196,111,251,63,255,0,248,43,23,254,189,55,246,200,175,191,232,3,249,161,212,127,106,63,11,124,116,248,53,251,16,255,0,193,54,254,7,252,98,253,136,63,105,47,218,91,227,103,135,215,227,95,237,117,30,131,241,135,78,248,5,251,52,254,217,16,124,20,159,192,255,0,21,255,0,108,189,63,224,223,196,79,134,159,0,252,71,99,241,119,195,255,0,25,127,106,63,26,120,183,254,18,248,180,127,133,126,35,240,111,196,15,4,124,60,253,167,124,15,175,93,120,87,198,190,27,215,227,208,79,11,254,216,223,0,239,126,6,124,35,253,155,255,0,107,15,218,135,196,30,24,253,172,127,96,223,219,124,126,201,154,143,237,77,251,51,252,85,240,95,237,69,226,15,217,235,226,102,137,251,30,254,208,254,56,240,207,237,13,241,171,226,47,198,111,134,250,141,191,141,60,63,39,236,93,107,241,195,225,231,198,143,137,31,16,190,17,233,190,30,210,254,40,120,119,226,207,136,228,240,183,129,180,111,5,159,24,120,67,250,94,162,128,63,154,31,135,94,60,248,55,224,251,223,130,95,180,205,230,167,251,48,120,115,224,119,133,63,224,179,223,20,126,49,254,208,127,183,151,192,136,231,248,77,251,13,254,208,58,143,196,207,248,35,247,199,79,134,186,135,237,109,165,222,120,175,226,215,140,124,55,240,147,195,247,63,29,190,39,248,23,224,158,191,127,7,143,117,173,43,196,127,24,254,31,106,183,55,90,133,191,142,124,95,168,248,118,207,159,253,167,62,62,124,44,139,246,113,253,188,127,104,15,10,254,209,95,0,60,27,240,203,227,199,252,22,251,254,9,93,226,63,217,151,246,151,241,207,137,188,57,226,175,217,199,226,7,252,43,11,95,248,35,197,135,140,60,125,225,77,123,78,248,143,225,253,63,227,87,195,255,0,8,248,131,246,119,248,238,254,43,135,195,254,43,176,251,44,95,0,252,109,101,117,173,232,179,120,119,89,187,210,255,0,167,234,40,3,249,66,253,176,62,53,127,194,171,210,255,0,105,73,191,109,47,218,239,246,64,248,57,255,0,5,7,212,255,0,107,255,0,248,36,111,194,95,3,91,107,190,26,255,0,133,37,240,39,196,127,178,207,236,195,255,0,5,28,248,87,251,76,124,18,253,170,124,11,240,39,226,191,237,63,117,227,127,140,95,15,236,52,207,218,79,227,52,223,21,53,219,63,24,104,62,29,211,181,159,130,94,45,240,22,147,125,109,31,195,189,87,226,55,138,185,255,0,142,255,0,16,126,1,126,213,62,7,253,139,191,103,15,25,254,223,127,177,7,143,191,106,63,143,191,240,81,255,0,218,155,90,184,253,175,191,101,173,55,193,118,31,12,227,248,183,227,31,216,147,254,10,55,240,155,246,31,248,177,240,210,255,0,72,241,60,22,158,38,253,167,254,18,233,58,199,236,1,164,91,104,214,158,59,215,190,33,248,31,196,49,252,32,182,212,60,99,113,125,226,143,135,126,46,241,47,245,187,69,0,127,44,90,127,237,65,225,95,218,35,246,238,255,0,130,101,254,216,127,31,83,195,255,0,179,95,138,188,83,251,79,252,58,253,152,62,10,124,6,248,193,227,77,58,77,127,194,94,42,183,255,0,130,85,254,217,190,55,253,175,223,225,110,191,227,75,29,6,225,188,65,39,237,161,251,101,124,14,253,156,126,36,232,186,78,135,109,115,255,0,11,67,246,34,240,166,131,226,137,219,197,111,163,248,59,194,73,251,5,254,219,31,0,172,254,40,93,248,183,226,47,252,20,71,254,9,193,227,111,217,167,193,127,180,255,0,252,22,7,226,55,195,95,217,251,195,41,224,191,17,254,213,95,13,126,38,124,81,255,0,130,138,252,87,212,62,28,254,212,183,62,43,176,248,141,226,251,237,115,224,253,183,236,185,227,31,218,155,92,213,188,111,161,232,191,13,252,61,224,239,133,31,23,111,124,67,227,9,252,77,224,184,181,63,24,248,115,250,158,162,128,63,148,47,248,39,204,254,59,211,62,20,127,193,185,22,30,25,241,119,252,46,95,217,79,198,191,240,169,252,89,224,79,29,143,136,186,63,196,141,99,224,151,237,29,225,47,248,36,55,252,20,123,193,31,181,47,236,241,175,235,250,143,136,191,181,191,225,0,255,0,132,183,251,26,239,193,122,44,113,120,146,95,4,107,222,18,248,171,224,173,91,85,240,103,133,180,95,133,30,6,182,244,15,248,40,39,252,50,197,239,252,63,103,246,92,208,127,225,159,238,255,0,225,82,127,193,0,127,103,63,248,71,63,103,77,35,254,21,212,255,0,240,172,191,225,157,255,0,225,230,159,17,60,21,253,137,240,142,207,63,240,134,127,194,11,255,0,11,11,246,104,215,116,175,35,79,182,255,0,132,99,254,19,143,2,234,118,159,97,254,211,208,39,159,250,125,175,128,63,224,172,95,242,139,47,248,41,103,253,152,7,237,145,255,0,172,235,241,26,128,15,248,38,159,252,155,175,196,111,251,63,255,0,248,43,23,254,189,55,246,200,175,191,235,249,129,253,157,191,224,189,95,240,73,239,216,183,75,253,160,63,102,143,218,95,246,172,255,0,133,109,241,179,225,183,237,255,0,255,0,5,59,255,0,132,215,193,95,240,163,63,105,47,24,255,0,98,255,0,194,99,255,0,5,28,253,170,60,125,225,207,248,168,252,1,240,119,85,210,117,47,180,248,75,197,90,13,223,250,37,252,254,79,219,254,207,63,149,115,20,208,199,244,7,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,3,247,250,138,252,1,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,160,15,223,234,43,240,7,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,63,127,168,175,192,31,248,138,59,254,8,81,255,0,71,205,255,0,154,205,251,97,255,0,244,62,209,255,0,17,71,127,193,10,63,232,249,191,243,89,191,108,63,254,135,218,0,253,254,162,191,0,127,226,40,239,248,33,71,253,31,55,254,107,55,237,135,255,0,208,251,71,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,3,247,250,190,0,255,0,130,177,127,202,44,191,224,165,159,246,96,31,182,71,254,179,175,196,106,248,3,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,181,242,7,252,20,39,254,14,60,255,0,130,49,252,113,253,129,127,110,31,130,159,11,127,108,159,248,74,62,38,252,96,253,144,63,105,111,133,191,14,188,53,255,0,12,243,251,85,232,191,240,145,120,239,226,7,193,143,26,248,79,194,58,23,246,207,136,190,6,218,105,250,79,218,252,65,171,233,246,255,0,106,190,187,181,179,183,251,71,155,117,113,12,8,242,40,7,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4305; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

