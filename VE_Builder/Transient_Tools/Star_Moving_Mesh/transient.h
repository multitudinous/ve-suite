#ifndef TRANSIENT_H
#define TRANSIENT_H
class Transient
{
   public:
      Transient( void );
      ~Transient( void );
      Transient( Transient * );
      
      int num_time_steps;
      int post_frequency;
      int begin_step;
      
      void writeScript( void );
};
#endif 
