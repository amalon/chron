/*
 * chron/time.h
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
 * Class for a chronological time.
 *
 */

#ifndef _CHRON_TIME_H_
#define _CHRON_TIME_H_

#include <inttypes.h>
#include <limits>
#include <ostream>

#include "common.h"

namespace chron
{
	typedef int64_t Instant;

	static const Instant instant_inf = std::numeric_limits<Instant>::min();
	static const Instant instant_sup = std::numeric_limits<Instant>::max();

	/**
	 * Range of integer values.
	 */
	class Range
	{
		Instant m_min, m_max;
	public:
		Range()
		{
		}

		static Instant term_to_instant(PlTerm &term)
		{
			int type = term.type();
			if (type == PL_ATOM) {
				PlAtom atom = (PlAtom)term;
				if (atom == "sup")
					return instant_sup;
				if (atom == "inf")
					return instant_inf;
			}
			assert(type == PL_INTEGER && "non integer instant");
			return (long)term;
		}

		Range(PlTerm range)
		{
			PlTerm min, max;
			PlCall("range_to_minmax",
			       PlTermv(range, min, max));

			m_min = term_to_instant(min);
			m_max = term_to_instant(max);
		}

		static std::ostream &ost_instant(std::ostream &ost, Instant i)
		{
			if (i == instant_inf)
				return ost << "inf";
			else if (i == instant_sup)
				return ost << "sup";
			else
				return ost << i;
		}

		friend std::ostream &operator <<(std::ostream &ost, const Range &r)
		{
			ost_instant(ost, r.m_min);
			if (r.m_max != r.m_min) {
				ost << "..";
				ost_instant(ost, r.m_max);
			}
			return ost;
		}
	};

	/**
	 * Union of ranges.
	 */
	class Domain
	{
		unsigned int m_num_ranges;
		Range *m_ranges;

	public:
		Domain()
		{
			m_num_ranges = 0;
		}

		Domain(const Domain &other)
		: m_num_ranges(other.m_num_ranges)
		{
			if (m_num_ranges) {
				m_ranges = new Range[m_num_ranges];
				for (unsigned int i = 0; i < m_num_ranges; ++i)
					m_ranges[i] = other.m_ranges[i];
			} else {
				m_ranges = NULL;
			}
		}

		~Domain()
		{
			delete [] m_ranges;
		}

		/**
		 * Construct from a prolog domain.
		 * @param dom	prolog domain.
		 */
		Domain(PlTerm &dom)
		{
			unsigned int i;
			PlTerm ranges, count;
			PlCall("domain_to_ranges", PlTermv(dom, ranges, count));
			m_num_ranges = (int)count;
			m_ranges = new Range[m_num_ranges];

			PlTerm range;
			PlQuery q("member", PlTermv(range, ranges));
			for (i = 0; i < m_num_ranges; ++i) {
				/* should be enough for another range */
				if (!q.next_solution()) {
					m_num_ranges = 0;
					delete [] m_ranges;
					m_ranges = NULL;
					return;
				}
				m_ranges[i] = Range(range);
			}
			/* shouldn't be any ranges spare */
			if (q.next_solution()) {
				m_num_ranges = 0;
				delete [] m_ranges;
				m_ranges = NULL;
			}
		}

		friend std::ostream &operator <<(std::ostream &ost, const Domain &d)
		{
			unsigned int i;
			for (i = 0; i < d.m_num_ranges; ++i) {
				if (i)
					ost << "\\/";
				ost << d.m_ranges[i];
			}
			return ost;
		}
	};

	/**
	 *
	 */
	class Time
	{
		PlTerm m_time;
	public:
		/**
		 * Construct from a prolog chron:time.
		 * @param time	chron:time constructed with mk_time.
		 */
		Time(PlTerm &time)
		: m_time(time)
		{
		}

		Domain domain() const
		{
			PlTerm dom;
			PlCall("time_to_domain", PlTermv(m_time, dom));
			return Domain(dom);
		}

		friend std::ostream &operator <<(std::ostream &ost, const Time &t)
		{
			return ost << t.domain();
		}
	};
};

#endif // _CHRON_TIME_H_
