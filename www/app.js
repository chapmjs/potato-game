// Potato Game browser glue: remember the player, send orders, keep focus.
(function () {
  function stored() { try { return localStorage.getItem('potato_token') || ''; } catch (e) { return ''; } }

  $(document).on('shiny:connected', function () {
    Shiny.setInputValue('stored_token', stored());
    Shiny.addCustomMessageHandler('store_token', function (t) {
      try { t ? localStorage.setItem('potato_token', t) : localStorage.removeItem('potato_token'); } catch (e) {}
    });
  });

  // The order carries the week it was typed for, so a double click or a slow
  // connection can never place an order in the wrong week.
  function submitOrder() {
    var b = $('#place_order');
    if (!b.length || b.prop('disabled')) return;
    var raw = $('#order_qty').val(), v = Number(raw);
    if (raw === '' || isNaN(v) || v < 0 || Math.floor(v) !== v) {
      $('#order_msg').text('Enter a whole number, 0 or more.');
      $('#order_qty').trigger('focus');
      return;
    }
    b.prop('disabled', true).text('Ordering...');
    Shiny.setInputValue('order_submit',
      { week: Number(b.data('week')), game: Number(b.data('game')), qty: v, n: Math.random() },
      { priority: 'event' });
  }
  $(document).on('click', '#place_order', submitOrder);
  $(document).on('keydown', '#order_qty', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); submitOrder(); }
  });
  $(document).on('keydown', '#join_name, #join_code', function (e) {
    if (e.key === 'Enter') { $(this).trigger('change'); $('#join').trigger('click'); }
  });
  $(document).on('keydown', '#admin_pw', function (e) {
    if (e.key === 'Enter') { $(this).trigger('change'); $('#admin_go').trigger('click'); }
  });

  // Plain buttons that send one value to the server.
  $(document).on('click', '[data-send]', function () {
    Shiny.setInputValue($(this).data('send'), { v: $(this).data('value'), n: Math.random() }, { priority: 'event' });
  });

  $(document).on('shiny:value', function (e) {
    if (e.name === 'play_board') setTimeout(function () { $('#order_qty').trigger('focus'); }, 60);
  });

  // Phone went to sleep or wifi dropped: come straight back to the same game.
  $(document).on('shiny:disconnected', function () {
    setTimeout(function () { location.reload(); }, 2500);
  });
})();
