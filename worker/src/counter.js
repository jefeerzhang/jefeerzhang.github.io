/* 每天提问次数计数。Durable Object 单线程串行,不会重复计数。 */
export class DailyCounter {
  constructor(state, env) {
    this.state = state;
    this.env = env;
  }

  async fetch(request) {
    const limit = Number(this.env.DAILY_LIMIT || 50);
    const perIp = Number(this.env.PER_IP_LIMIT || 10);

    let ip = 'unknown';
    try {
      const body = await request.json();
      if (body && typeof body.ip === 'string' && body.ip) ip = body.ip;
    } catch (e) {
      ip = 'unknown';
    }

    // 北京时间当天
    const now = new Date(Date.now() + 8 * 3600 * 1000);
    const day = now.toISOString().slice(0, 10);

    let data = await this.state.storage.get('counter');
    if (!data || data.day !== day) {
      data = { day, total: 0, ips: {} };
    }

    const used = data.ips[ip] || 0;
    if (data.total >= limit) {
      return reply({ allowed: false, reason: 'global', limit });
    }
    if (used >= perIp) {
      return reply({ allowed: false, reason: 'ip', limit: perIp });
    }

    data.total += 1;
    data.ips[ip] = used + 1;
    await this.state.storage.put('counter', data);

    return reply({ allowed: true, remaining: limit - data.total });
  }
}

function reply(body) {
  return new Response(JSON.stringify(body), {
    headers: { 'Content-Type': 'application/json; charset=utf-8' },
  });
}
