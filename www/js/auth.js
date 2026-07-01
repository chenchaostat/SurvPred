// survPred Shiny App — Auth Client Helpers
// Form validation and UI interactivity

$(document).ready(function() {

  // Password strength indicator
  $('#register-password').on('input', function() {
    var pw = $(this).val();
    var strength = 0;
    if (pw.length >= 8) strength++;
    if (pw.match(/[A-Z]/)) strength++;
    if (pw.match(/[0-9]/)) strength++;
    if (pw.match(/[^A-Za-z0-9]/)) strength++;

    var colors = ['#E74C3C', '#F39C12', '#2E86AB', '#27AE60'];
    var labels = ['Weak', 'Fair', 'Good', 'Strong'];
    var idx = Math.min(strength, 3);

    $('#pw-strength-bar').css('width', ((idx+1)*25) + '%');
    $('#pw-strength-bar').css('background-color', colors[idx]);
    $('#pw-strength-text').text(labels[idx]).css('color', colors[idx]);
  });

  // Confirm password match
  $('#register-confirm-password').on('input', function() {
    var pw = $('#register-password').val();
    var confirm = $(this).val();
    if (confirm.length > 0) {
      if (pw === confirm) {
        $(this).css('border-color', '#27AE60');
      } else {
        $(this).css('border-color', '#E74C3C');
      }
    } else {
      $(this).css('border-color', '');
    }
  });

  // Smooth scroll for anchor links
  $('a[href^="#"]').on('click', function(e) {
    e.preventDefault();
    var target = $($(this).attr('href'));
    if (target.length) {
      $('html, body').animate({ scrollTop: target.offset().top - 70 }, 600);
    }
  });

});
