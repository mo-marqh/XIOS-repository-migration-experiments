#ifndef __TIMELINE_EVENTS_HPP__
#define __TIMELINE_EVENTS_HPP__

#include <limits>

namespace xios
{
  const size_t timeLineEventbase = std::numeric_limits<size_t>::max()-100 ; 
  const size_t timelineEventNotifyChangeBufferSize = timeLineEventbase + 0  ; 
  const size_t timelineEventChangeBufferSize = timeLineEventbase + 1  ; 
}


#endif