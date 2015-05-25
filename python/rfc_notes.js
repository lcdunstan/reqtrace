// http://stackoverflow.com/questions/195951/change-an-elements-css-class-with-javascript
function hasClass(element, cls) {
    return element.className.match(new RegExp('(\\s|^)' + cls + '(\\s|$)'));
}
function addClass(element, cls) {
    if (!hasClass(element, cls)) {
        if (element.className.substr(-1) == " ") {
            element.className += cls;
        } else {
            element.className += " " + cls;
        }
    }
}
function removeClass(element, cls) {
    if (hasClass(element, cls)) {
        var reg = new RegExp('(\\s|^)' + cls + '(\\s|$)');
        element.className = element.className.replace(reg, ' ');
    }
}
