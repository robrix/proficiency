var Hover = function (element, target) {
  this.handleEvent = event => {
    switch (event.type) {
    case 'mouseover':
      target.classList.add("active");
      break;
    case 'mouseout':
      target.classList.remove("active");
      break;
    case 'click':
      target.scrollIntoView();
      console.log("hello");
      break;
    }
  };
  element.addEventListener('mouseover', this);
  element.addEventListener('mouseout', this);
  element.addEventListener('click', this);
};

var Toggle = function(element, target) {
  this.handleEvent = event => {
    if (target.classList.contains("hidden")) {
      target.classList.remove("hidden");
    } else {
      target.classList.add("hidden");
    }
  };
  element.addEventListener('change', this);
};

function run() {
  for (let li of document.querySelectorAll("#legend li")) {
    let costCentreId = li.getAttribute("data-id");
    let path = document.querySelector("#path-" + costCentreId);
    if (path) { li.hover = new Hover(li, path); }
  }
  for (let path of document.querySelectorAll("#graph path")) {
    let costCentreId = path.getAttribute("data-id");
    let li = document.querySelector("#legend-" + costCentreId);
    if (li) { path.hover = new Hover(path, li); }
  }

  let checkboxes = document.querySelectorAll("#legend li input");
  for (let input of checkboxes) {
    let costCentreId = input.getAttribute("data-id");
    let path = document.querySelector("#path-" + costCentreId);
    if (path) { input.toggle = new Toggle(input, path); }
  }

  let toggleAll = document.querySelector("#toggle-all");
  let toggleCheckbox = checkbox => {
    checkbox.checked = toggleAll.checked;
    var event = new Event('change');
    checkbox.dispatchEvent(event);
  };
  toggleAll.addEventListener('change', event => checkboxes.forEach(toggleCheckbox));

  let filterLegend = document.querySelector("#filter-legend");
  filterLegend.addEventListener('input', event => {
    if (filterLegend.value !== "") {
      document.querySelector("#legend").classList.add("filtered");
      document.querySelector("#graph").classList.add("filtered");
      for (let elem of document.querySelectorAll("#legend li, #graph path")) {
        if (elem.getAttribute("data-name").match(new RegExp(filterLegend.value, "i"))) {
          elem.classList.add("filtered");
        } else {
          elem.classList.remove("filtered");
        }
      }
    } else {
      document.querySelector("#legend").classList.remove("filtered");
      document.querySelector("#graph").classList.remove("filtered");
    }
  });
}
