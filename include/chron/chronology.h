/*
 * chron/chronology.h
 *
 * Copyright (C) 2012 James Hogan <james@albanarts.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
 * (in the file called COPYING).
 *
 *
 * Class for a chron chronology of chronological dates.
 *
 */

#ifndef _CHRON_CHRONOLOGY_H_
#define _CHRON_CHRONOLOGY_H_

#include "common.h"
#include "event.h"
#include "period.h"

namespace chron
{
	class Chronology
	{
		PlTerm m_db;
	public:
		int load(Event &e)
		{
			m_db = PlTerm();
			return !PlCall("process_db", PlTermv(e, m_db));
		}

		static bool lookup_event(Event *event, const char *name)
		{
			PlTerm t_event;
			PlTerm t_name(name);
			if (!PlCall("chron", "lookup_event",
				    PlTermv(t_name, t_event)))
				return false;
			*event = Event(t_event, t_name);
			return true;
		}

		static bool default_epoch(Event *event)
		{
			// use first event as epoch
			chron::Chronology::EventIterator epochit;
			if (!epochit)
				return false;
			*event = *epochit;
			return true;
		}

		class EventIterator
		{
			PlTerm m_name, m_event, m_time;
			PlQuery m_query;
			bool m_valid;

		public:
			EventIterator()
			: m_query("chron", "lookup_event",
				  PlTermv(m_name, m_event))
			{
				operator ++();
			}

			EventIterator(const Chronology &db)
			: m_query("chron", "lookup_db_event",
				  PlTermv(db.m_db, m_name, m_event, m_time))
			{
				operator ++();
			}

			EventIterator &operator ++()
			{
				m_valid = m_query.next_solution();
				return *this;
			}

			operator bool()
			{
				return m_valid;
			}

			Event operator *()
			{
				return Event(m_event, m_name);
			}

			Time time()
			{
				return Time(m_time);
			}
		};

		class PeriodIterator
		{
			PlTerm m_period, m_beginTime, m_endTime;
			PlQuery m_query;
			bool m_valid;

		public:
			PeriodIterator(Chronology &chron)
			: m_query("chron", "lookup_db_period", PlTermv(chron.m_db, m_period, m_beginTime, m_endTime))
			{
				operator ++();
			}

			PeriodIterator &operator ++()
			{
				m_valid = m_query.next_solution();
				return *this;
			}

			operator bool()
			{
				return m_valid;
			}

			Period operator *()
			{
				return Period(m_period);
			}

			Time begin_time()
			{
				return Time(m_beginTime);
			}

			Time end_time()
			{
				return Time(m_endTime);
			}
		};
	};
};

#endif // _CHRON_CHRONOLOGY_H_
